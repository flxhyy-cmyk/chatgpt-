;;; ADZZ - Quick Text Filter Command
;;; Function: Filter and select only TEXT and MTEXT objects within user-defined window

;; Global variable: Save last selection
(setq *ADZZ_LAST_SELECTION* nil)

;@name 快速筛选文字
;@group 文本编辑
;@desc 通过框选区域，自动筛选并只选中TEXT和MTEXT对象，过滤其他类型的图元
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


;;; ─────────────────────────────────────────────────────────────
;;; ADZZH - Batch Modify Text Height
;;; Function: Filter TEXT/MTEXT in selected area, batch change height
;;; ─────────────────────────────────────────────────────────────

;@name 批量修改字高
;@group 文本编辑
;@desc 框选区域内，批量修改TEXT和MTEXT的字高，支持常用字高快选和自定义输入
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
                           " object(s) → Height: "
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

;; ── DCL Dialog ───────────────────────────────────────────────

(defun show-height-dialog-adzzh (/ dcl-file dcl-id result chosen-height scope-val custom-val)
  (setq dcl-file (vl-filename-mktemp nil nil ".dcl"))
  (setq chosen-height "0")   ;; 0 = custom
  (setq custom-val   "3.5")  ;; default custom input
  (setq scope-val    "0")    ;; 0=both 1=TEXT only 2=MTEXT only

  ;; Write DCL file
  (setq f (open dcl-file "w"))
  (write-line "adzzh_dlg : dialog {" f)
  (write-line "  label = \"批量修改文字字高 / Batch Modify Text Height\";" f)

  ;; Preset buttons row
  (write-line "  : text { label = \"常用字高 / Presets:\"; }" f)
  (write-line "  : row {" f)
  (foreach h '("2.5" "3.5" "5.0" "7.0" "10.0")
    (write-line (strcat "    : button { key = \"h" h "\"; label = \"" h "\"; fixed_width = true; width = 6; }") f)
  )
  (write-line "  }" f)

  ;; Divider
  (write-line "  : text { label = \"─────────────────────────────────\"; }" f)

  ;; Custom input
  (write-line "  : row {" f)
  (write-line "    : text { label = \"自定义字高 / Custom:\"; }" f)
  (write-line "    : edit_box { key = \"custom_h\"; edit_width = 8; value = \"3.5\"; }" f)
  (write-line "  }" f)

  ;; Divider
  (write-line "  : text { label = \"─────────────────────────────────\"; }" f)

  ;; Scope radio
  (write-line "  : text { label = \"作用范围 / Apply to:\"; }" f)
  (write-line "  : radio_row {" f)
  (write-line "    : radio_button { key = \"scope_both\";  label = \"TEXT + MTEXT\"; value = \"1\"; }" f)
  (write-line "    : radio_button { key = \"scope_text\";  label = \"TEXT only\"; }" f)
  (write-line "    : radio_button { key = \"scope_mtext\"; label = \"MTEXT only\"; }" f)
  (write-line "  }" f)

  ;; Buttons
  (write-line "  : row {" f)
  (write-line "    : button { key = \"accept\"; label = \"确定 OK\"; is_default = true; fixed_width = true; alignment = centered; }" f)
  (write-line "    : button { key = \"cancel\"; label = \"取消 Cancel\"; is_cancel = true; fixed_width = true; alignment = centered; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)

  ;; Load dialog
  (setq dcl-id (load_dialog dcl-file))
  (if (not (new_dialog "adzzh_dlg" dcl-id))
    (progn (princ "\nError: Cannot load dialog.") nil)
    (progn
      ;; Preset buttons — clicking fills custom box and stores value
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

;; ── Apply Height ─────────────────────────────────────────────

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
       ;; Skip if height=0 (controlled by style) — warn instead
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
