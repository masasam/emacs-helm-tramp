# helm-tramp

tramp with helm interface

## Image


M-x helm-tramp  

![helm-tramp1](image/image1.png)


Display server list from your ~/.ssh/config in helm  


![helm-tramp2](image/image2.png)

Filter by helm  

![helm-tramp3](image/image3.png)

You can connect your server with tramp  

![helm-tramp4](image/image4.png)
![helm-tramp5](image/image5.png)
![helm-exit](image/exit.png)

## Requirements

- Emacs 24 or higher
- helm 1.7.7 or higher


## Sample Configuration

(setq tramp-default-method "ssh")  
(defalias 'exit-tramp 'tramp-cleanup-all-buffers)  
(define-key global-map (kbd "C-c s") 'helm-tramp)  
