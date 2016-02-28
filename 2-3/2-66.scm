(define (lookup given-key set-of-records)
  (if (null? set-of-records)
    false
    (let ((k (key (entry set-of-records))))
      (cond ((= given-key k)
             (entry set-of-records))
            ((< given-key k)
             (lookup given-key (left-branch set-of-records)))
            (else
              (lookup given-key (right-branch set-of-records)))))))
