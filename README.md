# helm-tramp

tramp with helm interface

## Image

![helm-tramp](image/helm-tramp.gif)


## Requirements

- helm 1.7.7 or higher


## Sample Configuration

(setq tramp-default-method "ssh")
(defalias 'exit-tramp 'tramp-cleanup-all-buffers)

#### `M-x helm-tramp`
