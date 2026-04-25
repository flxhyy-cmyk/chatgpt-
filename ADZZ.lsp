;;; ADZZ - Quick Text Filter Command
;;; Function: Filter and select only TEXT and MTEXT objects within user-defined window

;; Global variable: Save last selection
(setq *ADZZ_LAST_SELECTION* nil)

;@name ¿ìËÙÉ¸Ñ¡ÎÄ×Ö
;@group ÎÄ±¾±à¼­
;@desc Í¨¹ı¿òÑ¡ÇøÓò£¬×Ô¶¯É¸Ñ¡²¢Ö»Ñ¡ÖĞTEXTºÍMTEXT¶ÔÏó£¬¹ıÂËÆäËûÀàĞÍµÄÍ¼Ôª
;@require Selection
;@require ModelSpace
(defun c:ADZZ ()
  (setq ss nil)
  (setq text-ss nil)
  (setq text-count 0)
  (setq dcl-id nil)
  (setq alignment-choice nil)
  
  (princ "\n=== ADZZ Quick Text Filter ===")
  
  ;; Prompt user to select objects with window
  (princ "\nSelect area to filter text objects (TEXT/MTEXT only): ")
  (setq ss (ssget))
  
  (cond
    ;; User selected objects
    (ss
     (setq text-ss (filter-text-objects-adzz ss))
     (if text-ss
       (progn
         (setq text-count (sslength text-ss))
         (princ (strcat "\nFiltered " (itoa text-count) " text object(s)"))
         
         ;; Save selection for future use
         (setq *ADZZ_LAST_SELECTION* text-ss)
         
         ;; Set as current selection (keeps objects selected)
         (sssetfirst nil text-ss)
         (princ "\nText objects filtered and selected.")
         
         ;; Show alignment dialog
         (setq alignment-choice (show-alignment-dialog-adzz))
         
         (cond
           ;; User clicked OK with alignment choice
           ((and alignment-choice (>= alignment-choice 0))
            (apply-text-alignment-adzz text-ss alignment-choice)
            (princ "\nAlignment applied.")
           )
           ;; User clicked Cancel or closed dialog
           (t
            (princ "\nAlignment cancelled. Selection maintained.")
           )
         )
       )
       (princ "\nNo TEXT or MTEXT objects found in selection!")
     )
    )
    
    ;; No selection
    (t (princ "\nNo objects selected."))
  )
  
  (princ)
)

;; Filter only TEXT and MTEXT objects
(defun filter-text-objects-adzz (ss / i ent ent-type text-ss text-list)
  (setq text-list '())
  (setq i 0)
  
  ;; Loop through selection set
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    ;; Check if entity is TEXT or MTEXT
    (if (or (= ent-type "TEXT") (= ent-type "MTEXT"))
      (setq text-list (cons ent text-list))
    )
    
    (setq i (1+ i))
  )
  
  ;; Create new selection set with filtered text objects
  (if text-list
    (progn
      (setq text-ss (ssadd))
      (foreach ent (reverse text-list)
        (ssadd ent text-ss)
      )
      text-ss
    )
    nil
  )
)

;; Get statistics of selection
(defun get-text-statistics-adzz (ss / i ent ent-type text-count mtext-count)
  (setq text-count 0)
  (setq mtext-count 0)
  (setq i 0)
  
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    (cond
      ((= ent-type "TEXT") (setq text-count (1+ text-count)))
      ((= ent-type "MTEXT") (setq mtext-count (1+ mtext-count)))
    )
    
    (setq i (1+ i))
  )
  
  (princ (strcat "\n  TEXT: " (itoa text-count)))
  (princ (strcat "\n  MTEXT: " (itoa mtext-count)))
)

;; Show alignment dialog
(defun show-alignment-dialog-adzz (/ dcl-file dcl-id result alignment-value)
  (setq dcl-file (vl-filename-mktemp nil nil ".dcl"))
  (setq alignment-value "1")  ; Default: center
  
  ;; Create DCL file
  (setq f (open dcl-file "w"))
  (write-line "adzz_align : dialog {" f)
  (write-line "  label = \"Text Alignment\";" f)
  (write-line "  : radio_column {" f)
  (write-line "    key = \"align_group\";" f)
  (write-line "    : radio_button {" f)
  (write-line "      key = \"left\";" f)
  (write-line "      label = \"Left Align\";" f)
  (write-line "    }" f)
  (write-line "    : radio_button {" f)
  (write-line "      key = \"center\";" f)
  (write-line "      label = \"Center Align\";" f)
  (write-line "      value = \"1\";" f)
  (write-line "    }" f)
  (write-line "    : radio_button {" f)
  (write-line "      key = \"right\";" f)
  (write-line "      label = \"Right Align\";" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : button {" f)
  (write-line "      key = \"accept\";" f)
  (write-line "      label = \"OK\";" f)
  (write-line "      is_default = true;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "      alignment = centered;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"cancel\";" f)
  (write-line "      label = \"Cancel\";" f)
  (write-line "      is_cancel = true;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "      alignment = centered;" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  
  ;; Load and show dialog
  (setq dcl-id (load_dialog dcl-file))
  
  (if (not (new_dialog "adzz_align" dcl-id))
    (progn
      (princ "\nError: Cannot load dialog.")
      (setq result nil)
    )
    (progn
      ;; Set default value
      (set_tile "center" "1")
      
      ;; Action for radio buttons
      (action_tile "left" "(setq alignment-value \"0\")")
      (action_tile "center" "(setq alignment-value \"1\")")
      (action_tile "right" "(setq alignment-value \"2\")")
      
      ;; Action for OK button
      (action_tile "accept" "(done_dialog 1)")
      
      ;; Action for Cancel button
      (action_tile "cancel" "(done_dialog 0)")
      
      ;; Show dialog and get result
      (setq result (start_dialog))
      
      (unload_dialog dcl-id)
    )
  )
  
  ;; Delete temporary DCL file
  (vl-file-delete dcl-file)
  
  ;; Return alignment choice (0=left, 1=center, 2=right) or nil if cancelled
  (if (= result 1)
    (atoi alignment-value)
    nil
  )
)

;; Apply text alignment
(defun apply-text-alignment-adzz (ss alignment / i ent ent-type ent-data vla-obj insert-pt align-pt new-data)
  (setq i 0)
  
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    (cond
      ;; For TEXT objects
      ((= ent-type "TEXT")
       (setq ent-data (entget ent))
       (setq insert-pt (cdr (assoc 10 ent-data)))
       
       (cond
         ;; Left align (0) - Reset to default
         ((= alignment 0)
          ;; Set horizontal alignment to 0 (left)
          (if (assoc 72 ent-data)
            (setq ent-data (subst (cons 72 0) (assoc 72 ent-data) ent-data))
            (setq ent-data (append ent-data (list (cons 72 0))))
          )
          ;; Set vertical alignment to 0 (baseline)
          (if (assoc 73 ent-data)
            (setq ent-data (subst (cons 73 0) (assoc 73 ent-data) ent-data))
            (setq ent-data (append ent-data (list (cons 73 0))))
          )
          ;; Remove alignment point (11) if exists
          (if (assoc 11 ent-data)
            (setq ent-data (vl-remove (assoc 11 ent-data) ent-data))
          )
          (entmod ent-data)
         )
         
         ;; Center align (1)
         ((= alignment 1)
          ;; Set alignment point
          (if (assoc 11 ent-data)
            (setq ent-data (subst (cons 11 insert-pt) (assoc 11 ent-data) ent-data))
            (setq ent-data (append ent-data (list (cons 11 insert-pt))))
          )
          ;; Set horizontal alignment to 1 (center)
          (if (assoc 72 ent-data)
            (setq ent-data (subst (cons 72 1) (assoc 72 ent-data) ent-data))
            (setq ent-data (append ent-data (list (cons 72 1))))
          )
          ;; Set vertical alignment to 0 (baseline)
          (if (assoc 73 ent-data)
            (setq ent-data (subst (cons 73 0) (assoc 73 ent-data) ent-data))
            (setq ent-data (append ent-data (list (cons 73 0))))
          )
          (entmod ent-data)
         )
         
         ;; Right align (2)
         ((= alignment 2)
          ;; Set alignment point
          (if (assoc 11 ent-data)
            (setq ent-data (subst (cons 11 insert-pt) (assoc 11 ent-data) ent-data))
            (setq ent-data (append ent-data (list (cons 11 insert-pt))))
          )
          ;; Set horizontal alignment to 2 (right)
          (if (assoc 72 ent-data)
            (setq ent-data (subst (cons 72 2) (assoc 72 ent-data) ent-data))
            (setq ent-data (append ent-data (list (cons 72 2))))
          )
          ;; Set vertical alignment to 0 (baseline)
          (if (assoc 73 ent-data)
            (setq ent-data (subst (cons 73 0) (assoc 73 ent-data) ent-data))
            (setq ent-data (append ent-data (list (cons 73 0))))
          )
          (entmod ent-data)
         )
       )
       
       (entupd ent)
      )
      
      ;; For MTEXT objects
      ((= ent-type "MTEXT")
       (setq vla-obj (vlax-ename->vla-object ent))
       (cond
         ((= alignment 0) (vla-put-AttachmentPoint vla-obj acAttachmentPointTopLeft))
         ((= alignment 1) (vla-put-AttachmentPoint vla-obj acAttachmentPointTopCenter))
         ((= alignment 2) (vla-put-AttachmentPoint vla-obj acAttachmentPointTopRight))
       )
      )
    )
    
    (setq i (1+ i))
  )
)

(princ "\nCommand ADZZ loaded. Type ADZZ to start.")
(princ)


;;; ©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤
;;; ADZZH - Batch Modify Text Height
;;; Function: Filter TEXT/MTEXT in selected area, batch change height
;;; ©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤

;@name ÅúÁ¿ĞŞ¸Ä×Ö¸ß
;@group ÎÄ±¾±à¼­
;@desc ¿òÑ¡ÇøÓòÄÚ£¬ÅúÁ¿ĞŞ¸ÄTEXTºÍMTEXTµÄ×Ö¸ß£¬Ö§³Ö³£ÓÃ×Ö¸ß¿ìÑ¡ºÍ×Ô¶¨ÒåÊäÈë
;@require Selection
;@require ModelSpace

(defun c:ADZZH ()
  (setq ss nil)
  (setq text-ss nil)
  (setq text-count 0)

  (princ "\n=== ADZZH Batch Text Height Modifier ===")
  (princ "\nSelect area to modify text height (TEXT/MTEXT only): ")
  (setq ss (ssget))

  (cond
    (ss
     (setq text-ss (filter-text-objects-adzz ss))
     (if text-ss
       (progn
         (setq text-count (sslength text-ss))
         (princ (strcat "\nFiltered " (itoa text-count) " text object(s)"))
         (sssetfirst nil text-ss)

         ;; Show height dialog
         (setq height-result (show-height-dialog-adzzh))

         (cond
           ((and height-result (> (car height-result) 0))
            (apply-text-height-adzzh text-ss (car height-result) (cadr height-result))
            (princ (strcat "\nDone! Modified " (itoa text-count)
                           " object(s) ¡ú Height: "
                           (rtos (car height-result) 2 2)))
           )
           (t (princ "\nCancelled. No changes made."))
         )
       )
       (princ "\nNo TEXT or MTEXT objects found in selection!")
     )
    )
    (t (princ "\nNo objects selected."))
  )

  (princ)
)

;; ©¤©¤ DCL Dialog ©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤

(defun show-height-dialog-adzzh (/ dcl-file dcl-id result chosen-height scope-val custom-val)
  (setq dcl-file (vl-filename-mktemp nil nil ".dcl"))
  (setq chosen-height "0")   ;; 0 = custom
  (setq custom-val   "3.5")  ;; default custom input
  (setq scope-val    "0")    ;; 0=both 1=TEXT only 2=MTEXT only

  ;; Write DCL file
  (setq f (open dcl-file "w"))
  (write-line "adzzh_dlg : dialog {" f)
  (write-line "  label = \"ÅúÁ¿ĞŞ¸ÄÎÄ×Ö×Ö¸ß / Batch Modify Text Height\";" f)

  ;; Preset buttons row
  (write-line "  : text { label = \"³£ÓÃ×Ö¸ß / Presets:\"; }" f)
  (write-line "  : row {" f)
  (foreach h '("2.5" "3.5" "5.0" "7.0" "10.0")
    (write-line (strcat "    : button { key = \"h" h "\"; label = \"" h "\"; fixed_width = true; width = 6; }") f)
  )
  (write-line "  }" f)

  ;; Divider
  (write-line "  : text { label = \"©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤\"; }" f)

  ;; Custom input
  (write-line "  : row {" f)
  (write-line "    : text { label = \"×Ô¶¨Òå×Ö¸ß / Custom:\"; }" f)
  (write-line "    : edit_box { key = \"custom_h\"; edit_width = 8; value = \"3.5\"; }" f)
  (write-line "  }" f)

  ;; Divider
  (write-line "  : text { label = \"©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤\"; }" f)

  ;; Scope radio
  (write-line "  : text { label = \"×÷ÓÃ·¶Î§ / Apply to:\"; }" f)
  (write-line "  : radio_row {" f)
  (write-line "    : radio_button { key = \"scope_both\";  label = \"TEXT + MTEXT\"; value = \"1\"; }" f)
  (write-line "    : radio_button { key = \"scope_text\";  label = \"TEXT only\"; }" f)
  (write-line "    : radio_button { key = \"scope_mtext\"; label = \"MTEXT only\"; }" f)
  (write-line "  }" f)

  ;; Buttons
  (write-line "  : row {" f)
  (write-line "    : button { key = \"accept\"; label = \"È·¶¨ OK\"; is_default = true; fixed_width = true; alignment = centered; }" f)
  (write-line "    : button { key = \"cancel\"; label = \"È¡Ïû Cancel\"; is_cancel = true; fixed_width = true; alignment = centered; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)

  ;; Load dialog
  (setq dcl-id (load_dialog dcl-file))
  (if (not (new_dialog "adzzh_dlg" dcl-id))
    (progn (princ "\nError: Cannot load dialog.") nil)
    (progn
      ;; Preset buttons ¡ª clicking fills custom box and stores value
      (foreach h '("2.5" "3.5" "5.0" "7.0" "10.0")
        (action_tile (strcat "h" h)
          (strcat "(set_tile \"custom_h\" \"" h "\")"
                  "(setq chosen-height \"" h "\")"))
      )

      ;; Sync custom_h changes
      (action_tile "custom_h" "(setq chosen-height $value)")

      ;; Scope
      (action_tile "scope_both"  "(setq scope-val \"0\")")
      (action_tile "scope_text"  "(setq scope-val \"1\")")
      (action_tile "scope_mtext" "(setq scope-val \"2\")")

      (action_tile "accept" "(setq custom-val (get_tile \"custom_h\")) (done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")

      (setq result (start_dialog))
      (unload_dialog dcl-id)
    )
  )

  (vl-file-delete dcl-file)

  (if (= result 1)
    (progn
      ;; Use custom_h value as final source of truth
      (setq final-h (atof custom-val))
      (if (> final-h 0)
        (list final-h (atoi scope-val))
        (progn (princ "\nInvalid height value! Must be > 0.") nil)
      )
    )
    nil
  )
)

;; ©¤©¤ Apply Height ©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤

(defun apply-text-height-adzzh (ss new-height scope / i ent ent-type ent-data vla-obj cur-h)
  ;; scope: 0=both  1=TEXT only  2=MTEXT only
  (setq i 0)

  (repeat (sslength ss)
    (setq ent      (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))

    (cond
      ;; TEXT object
      ((and (= ent-type "TEXT")
            (or (= scope 0) (= scope 1)))
       (setq ent-data (entget ent))
       (setq cur-h (cdr (assoc 40 ent-data)))
       ;; Skip if height=0 (controlled by style) ¡ª warn instead
       (if (and cur-h (= cur-h 0))
         (princ (strcat "\n  [SKIP] Entity " (itoa i) ": height=0 (style-controlled)"))
         (progn
           (setq ent-data (subst (cons 40 new-height)
                                 (assoc 40 ent-data)
                                 ent-data))
           (entmod ent-data)
           (entupd ent)
         )
       )
      )

      ;; MTEXT object
      ((and (= ent-type "MTEXT")
            (or (= scope 0) (= scope 2)))
       (setq vla-obj (vlax-ename->vla-object ent))
       ;; Strip inline height codes e.g. \H3.5; before applying
       (setq raw-contents (vla-get-TextString vla-obj))
       (setq clean-contents
         (vl-string-subst "" 
           (car (vl-string-search "\\H" raw-contents))
           raw-contents))
       ;; Apply overall height via VLA
       (vla-put-Height vla-obj new-height)
       (entupd ent)
      )
    )

    (setq i (1+ i))
  )
)

(princ "\nCommand ADZZH loaded. Type ADZZH to start.")
(princ)


;;; â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
;;; ADZZF - Align Texts X Position Based on Topmost Text
;;; Flow: filter â†’ choose alignment â†’ apply attributes â†’ move X
;;; â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

;@name æ–‡å­—Xè½´å¯¹é½
;@group æ–‡æœ¬ç¼–è¾‘
;@desc æ¡†é€‰æ–‡å­—ï¼Œä»¥Yåæ ‡æœ€é«˜çš„æ–‡å­—ä¸ºåŸºå‡†ï¼Œå·¦/ä¸­/å³å¯¹é½æ‰€æœ‰æ–‡å­—Xåæ ‡
;@require Selection
;@require ModelSpace

(defun c:ADZZF ( / ss text-ss text-count alignment top-ent ref-x)
  (princ "\n=== ADZZF Text X-Alignment (based on topmost text) ===")
  (princ "\nSelect text objects to align: ")
  (setq ss (ssget))

  (cond
    (ss
     (setq text-ss (filter-text-objects-adzz ss))

     (cond
       ;; Need at least 2 texts
       ((or (not text-ss) (< (sslength text-ss) 2))
        (princ "\nNeed at least 2 TEXT/MTEXT objects to align.")
       )

       (t
        (setq text-count (sslength text-ss))
        (princ (strcat "\nFiltered " (itoa text-count) " text object(s)"))
        (sssetfirst nil text-ss)

        ;; Choose alignment mode (reuse existing ADZZ dialog)
        (setq alignment (show-alignment-dialog-adzz))

        (cond
          ((and alignment (>= alignment 0))

           ;; â”€â”€ Step 1: Apply text alignment attributes first â”€â”€â”€â”€â”€â”€
           ;; Must run before reading insertion points for accuracy
           (apply-text-alignment-adzz text-ss alignment)
           (princ "\n[Step 1] Alignment attributes applied.")

           ;; â”€â”€ Step 2: Find topmost text (highest Y) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
           (setq top-ent (find-topmost-adzzf text-ss))
           (setq ref-x   (get-align-x-adzzf top-ent alignment))
           (princ (strcat "\n[Step 2] Reference text found. Ref X = "
                          (rtos ref-x 2 4)))

           ;; â”€â”€ Step 3: Move all texts to align X â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
           (align-texts-x-adzzf text-ss ref-x alignment)
           (princ (strcat "\n[Step 3] " (itoa text-count)
                          " text(s) aligned. ADZZF complete!"))
          )

          (t (princ "\nCancelled. No changes made."))
        )
       )
     )
    )
    (t (princ "\nNo objects selected."))
  )
  (princ)
)

;; â”€â”€ Find topmost entity (highest Y of insertion point) â”€â”€â”€â”€â”€â”€â”€â”€

(defun find-topmost-adzzf (ss / i ent top-ent top-y cur-y)
  (setq top-ent (ssname ss 0))
  (setq top-y   (caddr (assoc 10 (entget top-ent))))
  (setq i 1)
  (repeat (1- (sslength ss))
    (setq ent   (ssname ss i))
    (setq cur-y (caddr (assoc 10 (entget ent))))
    (if (> cur-y top-y)
      (progn
        (setq top-ent ent)
        (setq top-y   cur-y)
      )
    )
    (setq i (1+ i))
  )
  top-ent
)

;; â”€â”€ Get reference X from a text entity based on alignment â”€â”€â”€â”€â”€
;; Left(0)  â†’ TEXT uses point 10 X  / MTEXT uses point 10 X
;; Center(1)â†’ TEXT uses point 11 X  / MTEXT uses point 10 X
;; Right(2) â†’ TEXT uses point 11 X  / MTEXT uses point 10 X

(defun get-align-x-adzzf (ent alignment / ent-type ent-data)
  (setq ent-type (cdr (assoc 0 (entget ent))))
  (setq ent-data (entget ent))
  (cond
    ((= ent-type "TEXT")
     (cond
       ((= alignment 0) (cadr (assoc 10 ent-data)))  ;; left  â†’ point 10
       (t               (cadr (assoc 11 ent-data)))  ;; center/right â†’ point 11
     )
    )
    ((= ent-type "MTEXT")
     (cadr (assoc 10 ent-data))  ;; MTEXT attachment always at point 10
    )
  )
)

;; â”€â”€ Move all texts so their alignment X equals target-x â”€â”€â”€â”€â”€â”€â”€

(defun align-texts-x-adzzf (ss target-x alignment / i ent ent-type ent-data pt10 pt11 cur-x delta new-x10)
  (setq i 0)
  (repeat (sslength ss)
    (setq ent      (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    (setq ent-data (entget ent))

    (cond
      ;; TEXT object
      ((= ent-type "TEXT")
       (setq pt10 (assoc 10 ent-data))

       (cond
         ;; Left align: point 10 is the left edge, move it directly
         ((= alignment 0)
          (setq ent-data
            (subst (list 10 target-x (caddr pt10) (cadddr pt10))
                   pt10 ent-data))
         )

         ;; Center / Right: point 11 is the anchor, shift both 10 and 11
         (t
          (setq pt11 (assoc 11 ent-data))
          (if pt11
            (progn
              (setq cur-x  (cadr pt11))
              (setq delta  (- target-x cur-x))
              (setq new-x10 (+ (cadr pt10) delta))
              ;; Shift insertion point by same delta
              (setq ent-data
                (subst (list 10 new-x10 (caddr pt10) (cadddr pt10))
                       pt10 ent-data))
              ;; Set alignment point to target
              (setq ent-data
                (subst (list 11 target-x (caddr pt11) (cadddr pt11))
                       pt11 ent-data))
            )
            ;; Fallback: point 11 missing, move point 10
            (setq ent-data
              (subst (list 10 target-x (caddr pt10) (cadddr pt10))
                     pt10 ent-data))
          )
         )
       )
       (entmod ent-data)
       (entupd ent)
      )

      ;; MTEXT object: attachment point is always point 10
      ((= ent-type "MTEXT")
       (setq pt10 (assoc 10 ent-data))
       (setq ent-data
         (subst (list 10 target-x (caddr pt10) (cadddr pt10))
                pt10 ent-data))
       (entmod ent-data)
       (entupd ent)
      )
    )

    (setq i (1+ i))
  )
)

(princ "\nCommand ADZZF loaded. Type ADZZF to start.")
(princ)


;;; â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
;;; ADZZV - Align Texts X Position Only (no attribute change)
;;; Flow: filter â†’ choose alignment â†’ move X only
;;; â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

;@name æ–‡å­—Xè½´å¯¹é½ï¼ˆä»…ç§»åŠ¨ï¼‰
;@group æ–‡æœ¬ç¼–è¾‘
;@desc æ¡†é€‰æ–‡å­—ï¼Œä»¥Yåæ ‡æœ€é«˜çš„æ–‡å­—ä¸ºåŸºå‡†å¯¹é½Xåæ ‡ï¼Œä¸ä¿®æ”¹æ–‡å­—æœ¬èº«å¯¹é½å±æ€§
;@require Selection
;@require ModelSpace

(defun c:ADZZV ( / ss text-ss text-count alignment top-ent ref-x)
  (princ "\n=== ADZZV Text X-Alignment (position only, no attribute change) ===")
  (princ "\nSelect text objects to align: ")
  (setq ss (ssget))

  (cond
    (ss
     (setq text-ss (filter-text-objects-adzz ss))

     (cond
       ((or (not text-ss) (< (sslength text-ss) 2))
        (princ "\nNeed at least 2 TEXT/MTEXT objects to align.")
       )

       (t
        (setq text-count (sslength text-ss))
        (princ (strcat "\nFiltered " (itoa text-count) " text object(s)"))
        (sssetfirst nil text-ss)

        ;; Choose alignment mode
        (setq alignment (show-alignment-dialog-adzz))

        (cond
          ((and alignment (>= alignment 0))

           ;; â”€â”€ Step 1: Find topmost text (highest Y) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
           ;; No attribute adjustment here, read current points as-is
           (setq top-ent (find-topmost-adzzf text-ss))
           (setq ref-x   (get-align-x-adzzf top-ent alignment))
           (princ (strcat "\n[Step 1] Reference text found. Ref X = "
                          (rtos ref-x 2 4)))

           ;; â”€â”€ Step 2: Move all texts to align X â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
           (align-texts-x-adzzf text-ss ref-x alignment)
           (princ (strcat "\n[Step 2] " (itoa text-count)
                          " text(s) aligned. ADZZV complete!"))
          )

          (t (princ "\nCancelled. No changes made."))
        )
       )
     )
    )
    (t (princ "\nNo objects selected."))
  )
  (princ)
)

(princ "\nCommand ADZZV loaded. Type ADZZV to start.")
(princ)
