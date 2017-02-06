# helm-tramp

tramp with helm interface

## Image

![helm-tramp1](image/image1.png)
![helm-tramp1](image/image2.png)
![helm-tramp1](image/image3.png)


## Requirements

- Emacs 24 or higher
- helm 1.7.7 or higher


## Sample Configuration

(setq tramp-default-method "ssh")  
(defalias 'exit-tramp 'tramp-cleanup-all-buffers)  
(define-key global-map (kbd "C-c s") 'helm-tramp)  

#### `M-x helm-tramp`
