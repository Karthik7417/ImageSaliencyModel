require('magick','imager')

img <- load.image(file = 'tree.jpg')
img.df <- as.data.frame(img)

hsalMapNM = hMapClrOppGGDNM(img * 255)

[ioMSalImgNM, ioMSalMapNM]  = cSalMapComb(hsalMapNM, double(img), 1, 1, -1, [0.25, 0.25, 0.25, 0.25, 1.0]);

