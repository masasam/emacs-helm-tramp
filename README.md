# helm-tramp

tramp with helm interface

## Image

![helm-tramp](image/helm-tramp.gif)


## Requirements

- helm 1.7.7 or higher


## Sample Configuration

(setq tramp-default-method "ssh")  
(defalias 'exit-tramp 'tramp-cleanup-all-buffers)  
(define-key global-map (kbd "C-c s") 'helm-tramp)  

#### `M-x helm-tramp`
