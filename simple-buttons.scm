(define (insert-above-layer image old-layer new-layer . adjust-option)
  (let* ((parent-layer (car (gimp-item-get-parent old-layer)))
         (parent-layer-or-root (if (= parent-layer -1) 0 parent-layer))
         (position (car (gimp-image-get-item-position image old-layer)))
         (adjust (if (null? adjust-option) 0 (car adjust-option))))
    (gimp-image-insert-layer image new-layer parent-layer-or-root (+ position adjust))))
    
(define (insert-below-active-layer image new-layer)
  (let* ((active-layer (car (gimp-image-get-active-layer image))))
    (insert-above-layer image active-layer new-layer 1)))

(define (copy-and-merge-visible-layers image layers)
  (define (is-visible layer) (if (> (car (gimp-item-get-visible layer)) 0) #t #f))
  (define (copy-layer layer) (car (gimp-layer-copy layer 1)))
  (let* ((rev (reverse (filter is-visible layers)))
         (bottom (copy-layer (car rev)))
         (rest (cdr rev)))
    (insert-below-active-layer image bottom)
    (for-each (lambda (layer)
                (let ((copy (copy-layer layer)))
                  (gimp-message (number->string layer))
                  (insert-above-layer image bottom copy)
                  (set! bottom (car (gimp-image-merge-down image copy EXPAND-AS-NECESSARY)))))
              rest)
    bottom))

(define (make-layer-group-button active-image active-group active-parent active-text)
  (define old-text (car (gimp-text-layer-get-text active-text)))
  (define (set-active-text text) (gimp-text-layer-set-text active-text text))
  (define (get-position layer) (car (gimp-image-get-item-position active-image layer)))
  (define position (get-position (if (= active-parent 0) active-group active-parent)))
  (define (children layer) (vector->list (cadr (gimp-item-get-children layer))))
  (gimp-message "Doing it right??")
  (gimp-image-undo-group-start active-image)
  (let ((new-button (copy-and-merge-visible-layers active-image (children active-group))))
    (set-active-text (number->string (+ 1 (string->number old-text))))
    (gimp-item-set-name new-button old-text))
  (gimp-image-set-active-layer active-image active-group)
  (gimp-image-undo-group-end active-image))

(define (script-fu-simple-buttons active-image active-layer)
  (define (parent-layer) (car (gimp-item-get-parent active-layer)))
  (define parent-layer-or-root (if (= (parent-layer) -1) 0 (parent-layer)))
  (define (is-group layer) (if (> (car (gimp-item-is-group layer)) 0) #t #f))
  (define (is-text layer) (if (> (car (gimp-item-is-text-layer layer)) 0) #t #f))
  (define (is-visible layer) (if (> (car (gimp-item-get-visible layer)) 0) #t #f))
  (define (children layer) (vector->list (cadr (gimp-item-get-children layer))))
  (define (visible-children layer) (filter is-visible (children layer)))
  (if (is-group active-layer)
    (let ((text-children (filter is-text (visible-children active-layer))))
      (if (= (length text-children) 1)
        (make-layer-group-button active-image active-layer parent-layer-or-root (car text-children))
        (gimp-message "Layer group must contain a single visible text layer")))
    (gimp-message "Layer group must be active"))
  (gimp-displays-flush))
    
(script-fu-register "script-fu-simple-buttons"
  "Simple buttons"
  "Not sure how to define this yet"
  ""
  ""
  ""
  "RGB* INDEXED* GRAY*"
  SF-IMAGE    "Image"    0
  SF-DRAWABLE "Drawable" 0)
    
(script-fu-menu-register "script-fu-simple-buttons" "<Image>/Filters")
