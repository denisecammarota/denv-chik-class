library(magick)

files  <- c('figures/31.jpg', 'figures/33.jpg', 'figures/35.jpg')
labels <- c('(a)', '(b)', '(c)')

imgs <- image_read(files)
imgs <- image_scale(imgs, 'x1200')   # same height for all (use '1200' for same width if stacking)

imgs <- image_annotate(imgs, labels, size = 60, color = 'black',
                       gravity = 'northwest', location = '+20+10')

fig <- image_append(imgs)            # side by side; image_append(imgs, stack = TRUE) for vertical
image_write(fig, 'figures/figure_S4.jpg', format = 'jpg', quality = 95)

files  <- c('figures/15.jpg', 'figures/26.jpg', 'figures/42.jpg', 'figures/53.jpg')
labels <- c('(a)', '(b)', '(c)', '(d)')

imgs <- image_read(files)
imgs <- image_scale(imgs, 'x1200')   # same height for all
imgs <- image_annotate(imgs, labels, size = 60, color = 'black',
                       gravity = 'northwest', location = '+20+10')

top    <- image_append(imgs[1:2])    # (a) (b)
bottom <- image_append(imgs[3:4])    # (c) (d)

# make both rows the same width before stacking
w <- min(image_info(top)$width, image_info(bottom)$width)
fig <- image_append(c(image_scale(top, as.character(w)),
                      image_scale(bottom, as.character(w))), stack = TRUE)

image_write(fig, 'figures/figure_S5.jpg', format = 'jpg', quality = 95)
