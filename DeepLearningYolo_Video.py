
### https://www.intelligenzaartificialeitalia.net/post/object-recognition-e-l-object-predict-deep-learnign-e-python-esempio-pratico-in-13-righe-di-codice
#!/usr/bin/env python
# coding: utf-8

from imageai.Detection import VideoObjectDetection
import os

# execution_path = os.getcwd()
model_path = "D:/Federico/CityFLows/objectdetection/models/yolo-tiny.h5"
input_path = "D:/Federico/CityFLows/objectdetection/input/piazza_duca_aosta_25112021.mp4"
output_path = "D:/Federico/CityFLows/objectdetection/output/piazza_duca_aosta_25112021_detection.mp4"

detector = VideoObjectDetection()
detector.setModelTypeAsTinyYOLOv3()

# detector.setModelPath(os.path.join(model_path, "yolo-tiny.h5"))
detector.setModelPath(model_path)
detector.loadModel()

video_path = detector.detectObjectsFromVideo(input_file_path=input_path,
                                             output_file_path=output_path
                                             ,frames_per_second=20, log_progress=True)
print(video_path)