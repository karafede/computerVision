

from modelsAI import *
import utilsAI
import os
os.getcwd()
from time import sleep
output_path = "D:/Federico/CityFLows/objectdetection/output/prova_piazza_IDs/"


import os, sys, time, datetime, random
import torch
from torch.utils.data import DataLoader
from torchvision import datasets, transforms
from torch.autograd import Variable

import matplotlib.pyplot as plt


import matplotlib.patches as patches
from PIL import Image
from datetime import timedelta
import pandas as pd


config_path='config/yolov3.cfg'
weights_path='config/yolov3.weights'
class_path='config/coco.names'
img_size=416
conf_thres=0.3  # 0.8    0.3
nms_thres=0.2   # 0.4    0.2


# Load model and weights
model = Darknet(config_path, img_size=img_size)
model.load_weights(weights_path)
# model.cuda()
model.eval()
classes = utilsAI.load_classes(class_path)
# Tensor = torch.cuda.FloatTensor
Tensor = torch.FloatTensor



def detect_image(img):
    # scale and pad image
    ratio = min(img_size/img.size[0], img_size/img.size[1])
    imw = round(img.size[0] * ratio)
    imh = round(img.size[1] * ratio)
    img_transforms = transforms.Compose([ transforms.Resize((imh, imw)),
         transforms.Pad((max(int((imh-imw)/2),0), max(int((imw-imh)/2),0), max(int((imh-imw)/2),0), max(int((imw-imh)/2),0)),
                        (128,128,128)),
         transforms.ToTensor(),
         ])
    # convert image to Tensor
    image_tensor = img_transforms(img).float()
    image_tensor = image_tensor.unsqueeze_(0)
    input_img = Variable(image_tensor.type(Tensor))
    # run inference on the model and get detections
    with torch.no_grad():
        detections = model(input_img)
        detections = utilsAI.non_max_suppression(detections, 80, conf_thres, nms_thres)
    return detections[0]



## load an .mp4 video
# videopath = 'input/short_FK.mp4'
# videopath = 'input/piazza_duca_aosta_25112021.mp4'
# videopath = 'input/prova_piazza_Duca_Aosta_25112021.mp4'
# videopath = 'input/video_corridoio_camera_93_ROMA3.avi'
# videopath = 'input/video_corridoio_camera_94_ROMA3.avi'
# videopath = 'input/cam_94_lato_corto_bidirez.mp4'
# videopath = 'input/cam_94_lato_corto_monodirez_stretto.mp4'
# videopath = 'input/cam_94_lato_corto_bidirez_stretto.mp4'
videopath = 'input/cam_94_lato_corto_bidirez_corr_largo.mp4'

# figure = plt.figure()
import cv2
from IPython.display import clear_output

cmap = plt.get_cmap('tab20b')
colors = [cmap(i)[:3] for i in np.linspace(0, 1, 20)]

# initialize Sort object and video capture
from sort import *
vid = cv2.VideoCapture(videopath)
## to get the tmestamp
fps = vid.get(cv2.CAP_PROP_FPS)
mot_tracker = Sort()


# timestamps = [vid.get(cv2.CAP_PROP_POS_MSEC)]
timestamps = []

count = 0
all_IDs = pd.DataFrame([])
time0 = pd.date_range('2021-11-25 08:42:59', periods=1)
times = time0


# while(True):
for ii in range(4000):
    # timestamps.append(vid.get(cv2.CAP_PROP_POS_MSEC))
    ID_X = []
    ID_Y = []
    ID = []
    LABEL =[]
    FRAME = []
    print("---------------------------------------- frame number:", ii)
    ret, frame = vid.read()
    try:
        frame = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
        count += 1
        pilimg = Image.fromarray(frame)
        detections = detect_image(pilimg)
        timestamps.append(vid.get(cv2.CAP_PROP_POS_MSEC))
        AAA = list(set(timestamps))
        if (len(AAA) > 1):
            timestamp_diff = int(abs((AAA[-1]+ 1000 / fps) - (AAA[-2]+ 1000 / fps)))
        else:
            timestamp_diff = int((AAA[0] + 1000 / fps))
        # print('Frame difference:', timestamp_diff)  # value in milli seconds

        img = np.array(pilimg)
        pad_x = max(img.shape[0] - img.shape[1], 0) * (img_size / max(img.shape))
        pad_y = max(img.shape[1] - img.shape[0], 0) * (img_size / max(img.shape))
        unpad_h = img_size - pad_y
        unpad_w = img_size - pad_x

        if detections is not None:
            tracked_objects = mot_tracker.update(detections.cpu())
            # times = times + timestamp_diff * timedelta(microseconds=1000)  # 1 milli second
            times = times + 100 * timedelta(microseconds=1000)  # 1 milli second
            print("******************** timestamp:", times[0])
            unique_labels = detections[:, -1].cpu().unique()
            n_cls_preds = len(unique_labels)
            for x1, y1, x2, y2, obj_id, cls_pred in tracked_objects:
                ## detect OMLY persons
                if cls_pred == 0:
                    periods = len(tracked_objects)
                    box_h = int(((y2 - y1) / unpad_h) * img.shape[0])
                    box_w = int(((x2 - x1) / unpad_w) * img.shape[1])
                    y1 = int(((y1 - pad_y // 2) / unpad_h) * img.shape[0])
                    x1 = int(((x1 - pad_x // 2) / unpad_w) * img.shape[1])
                    ##### print("centroid X, Y, ID, label:", x1, y1, int(obj_id), int(cls_pred))
                    ID_X.append(x1)
                    ID_Y.append(y1)
                    ID.append(int(obj_id))
                    FRAME.append(ii)
                    LABEL.append(int(cls_pred))
                    timestamps.append(vid.get(cv2.CAP_PROP_POS_MSEC))
                    list_of_X_Y = list(zip(ID_X, ID_Y))
                    DF_people = pd.DataFrame(list_of_X_Y, columns=['X', 'Y'])
                    DF_people['ID'] = ID
                    # DF_people['diff_timestamp'] = timestamp_diff
                    DF_people['timestamp'] = str(times[0])
                    # DF_people['frame_count'] = count
                    # DF_people['label'] = LABEL
                    DF_people['frame_number'] = FRAME


                    color = colors[int(obj_id) % len(colors)]
                    color = [i * 255 for i in color]
                    cls = classes[int(cls_pred)]
                    cv2.rectangle(frame, (x1, y1), (x1+box_w, y1+box_h), color, 4)
                    cv2.rectangle(frame, (x1, y1-35), (x1+len(cls)*19+60, y1), color, -1)
                    cv2.putText(frame, cls + "-" + str(int(obj_id)), (x1, y1 - 10), cv2.FONT_HERSHEY_SIMPLEX, 1, (255,255,255), 3)

                    # save image
                    fig = plt.figure(figsize=(12, 8))
                    fig = plt.imshow(frame)
                    # plt.axis('off')

                    plt.savefig(output_path + str(ii) + ".jpg",
                                bbox_inches='tight', pad_inches=0.0)
                    plt.close()
        all_IDs = pd.concat([all_IDs, DF_people])

    except cv2.error:
        pass



# sort by 'timedate'
# DF_people.sort_values(['timedate'], ascending=True, inplace=True)
### save .csv file
all_IDs.to_csv('cam_94_lato_corto_bidirez_corr_largo.csv')

