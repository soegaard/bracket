#lang racket

(provide set-clipboard-bitmap)

(require racket/draw
         ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/atomic
         mred/private/wx/cocoa/image)

(import-class NSPasteboard NSArray)

(define (set-clipboard-bitmap bitmap)
  (start-atomic)
  (let ([pasteboard (tell NSPasteboard generalPasteboard)])
    (tell pasteboard clearContents)
    (let ([copied-objects (tell NSArray arrayWithObject:
                                (bitmap->image bitmap))])
      (tell pasteboard writeObjects: copied-objects)
      (end-atomic))))
