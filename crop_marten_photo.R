library(magick)

image_read('Marten_name.jpg') %>% image_crop('x1000+0+300') %>% image_resize('600x600') %>% image_write('Marten_name_cropped.jpg')