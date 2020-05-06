(in-package #:browse-cl)

(defun wrap-text (font text width)
  "Returns a list of lines"
  (let ((lines (list))
        (words (cl-ppcre:split "\\s" text))
        (curr-line ""))
    (loop for w in words do
          (let* ((new-line (if (> (length curr-line) 0) 
                               (concatenate 'string curr-line " " w)
                               w))
                 (line-width (sdl2-ttf:size-text font new-line)))
            (if (> line-width width)
                (progn
                  (push curr-line lines)
                  (setf curr-line w))
                (setf curr-line new-line))))
    (when (> (length curr-line) 0) (push curr-line lines))
    (remove-if (lambda (x) (= (length x) 0)) (reverse lines))))

(defmethod render-wrapped-text-to-atlas-manager
  ((a atlas-manager) font-name size text width &optional (r 255) (g 255) (b 255))
  "Render some text to an atlas-manager, store it in the atlas, then return
   the name of the texture, which can be used to look up the texture later.
   Remember to flush the atlas-manager before using this texture."
  (let ((data (render-wrapped-text font-name size text width r g b)))
    (load-tex-from-c-array a data)))

(defun render-wrapped-text (font-name size text width &optional (r 255) (g 255) (b 255))
  "Render some wrapped text, return a c-array which can be used to make a
   texture
   
   * font-name - the font path to use - this is looked up in the font cache
   * size - the font size to use
   * text - the text to insert into the texture
   * width - the width available to layout the text
   * r, g, b - the colour to render the text"

  (if (= 0 (length text)) (error "Text is empty"))

  ;; Wrap text into lines, find the line height, use that to calculate the
  ;; final texture height
  (let* ((font (load-font font-name size))
         (lines (wrap-text font text width))
         (line-height (multiple-value-bind (w h)
                        (sdl2-ttf:size-text font (car lines))
                        (declare (ignore w)) 
                        h))
         (height (* line-height (length lines)))
         ;; Allocate a surface to store the final texture
         (surface (sdl2:create-rgb-surface 
                    width height 32 
                    :r-mask #xff000000 :g-mask #x00ff0000 
                    :b-mask #x0000ff00 :a-mask #x000000ff))
         ;; Render all lines of text to a separate surface
         (line-surfaces (loop for l in lines collect (sdl2-ttf:render-utf8-blended font l r g b 255)))
         (ret-tmp nil)
         ;; Hold our return value
         (ret nil))
    (sdl2-ffi.functions:sdl-set-surface-blend-mode 
      surface sdl2-ffi:+sdl-blendmode-none+)
    (sdl2:fill-rect surface (sdl2:make-rect 0 0 width height) 
                    #x00ffffff)
    ;; Blit all line-surfaces onto surface
    (loop for ii from 0 for ls in line-surfaces do
          (sdl2:blit-surface ls (sdl2:make-rect 0 0 width line-height)
                             surface (sdl2:make-rect 0 (* ii line-height) width line-height)))
    ;; Convert surface to a tex
    (setf ret-tmp (make-c-array-from-pointer 
                (list width height) 
                :uint8-vec4 (sdl2:surface-pixels surface)))
    ;; Copy ret-tmp to a new c-array
    (setf ret (make-c-array nil :dimensions (list width height) :element-type :uint8-vec4))
    (blit-c-array ret ret-tmp)
    ;; Free all surfaces
    ;; Some reason if we free line-surfaces here we get a double free, probably
    ;; because render-utf8-blended uses a finalizer, so don't free those
    (sdl2:free-surface surface)
    ret))

