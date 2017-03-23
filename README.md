# helm-tramp [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

Tramp with helm interface for server and docker

## Screenshot

    M-x helm-tramp

![helm-tramp1](image/image1.png)

Display server list from your ~/.ssh/config with helm interface

![helm-tramp2](image/image2.png)

Filter by helm

![helm-tramp3](image/image3.png)

You can connect your server with tramp

![helm-tramp4](image/image4.png)

Selecting the list with sudo will lead to the server as root

![helm-tramp5](image/image5.png)

You can edit your server's nginx.conf on your emacs!

![docker-tramp](image/docker-tramp.png)

If you are using [docker-tramp](https://github.com/emacs-pe/docker-tramp.el), docker is also supplemented

![docker-tramp1](image/docker-tramp1.png)

You can edit docker container on your emacs!

![helm-exit](image/exit.png)

When you finish editing nginx.conf you clean the tramp buffer with `tramp-cleanup-all-buffers` command.  
Since I can not remember `tramp-cleanup-all-buffers` command I set a defalias called `exit-tramp`.  

## Requirements

- Emacs 24.3 or higher
- helm 2.0 or higher

## Installation

You can install `helm-tramp.el` from [MELPA](http://melpa.org) with package.el
(`M-x package-install helm-tramp`).  
You can install `docker-tramp.el` from [MELPA](http://melpa.org) with package.el
(`M-x package-install docker-tramp`).  

## Sample Configuration

	(setq tramp-default-method "ssh")
    (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
    (define-key global-map (kbd "C-c s") 'helm-tramp)

If the shell of the server is zsh it is recommended to connect with bash.  

    (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

If you want to specify the user name to connect with docker-tramp  

	(setq helm-tramp-docker-user "username")

[melpa-link]: http://melpa.org/#/helm-tramp
[melpa-badge]: http://melpa.org/packages/helm-tramp-badge.svg
[melpa-stable-link]: http://stable.melpa.org/#/helm-tramp
[melpa-stable-badge]: http://stable.melpa.org/packages/helm-tramp-badge.svg
