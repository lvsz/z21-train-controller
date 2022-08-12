#lang racket/base

(provide message
         message?
         message-id
         message-header
         message-body)

(struct message (id header body) #:prefab)

