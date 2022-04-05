
### https://www.intelligenzaartificialeitalia.net/post/object-recognition-e-l-object-predict-deep-learnign-e-python-esempio-pratico-in-13-righe-di-codice
#!/usr/bin/env python
# coding: utf-8

# In[1]:
from imageai.Detection import ObjectDetection

# In[2]:
detector = ObjectDetection()

# In[9]:
model_path = "D:/Federico/CityFLows/objectdetection/models/yolo-tiny.h5"
# model_path = "./models/resnet50_coco_best_v2.1.0.h5"
input_path = "D:/Federico/CityFLows/objectdetection/input/img_new.png"
output_path = "D:/Federico/CityFLows/objectdetection/output/newimage.jpg"

# In[4]:
detector.setModelTypeAsTinyYOLOv3()
# detector.setModelTypeAsRetinaNet()

# In[5]:
detector.setModelPath(model_path)

# In[7]:
detector.loadModel()

# In[13]:
detection = detector.detectObjectsFromImage(input_image=input_path, output_image_path=output_path)

# In[14]:
for eachItem in detection:
    print(eachItem["name"] , " : ", eachItem["percentage_probability"])