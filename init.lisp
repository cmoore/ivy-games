
(ql:quickload 'swank)
(swank:create-server :port 4000)
(ql:quickload 'ivy-games)
(in-package :ivy-games)
(start-server)
