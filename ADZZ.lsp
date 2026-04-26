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

;; Filter only TEXT, MTEXT, and ATTRIB objects (including block text)
(defun filter-text-objects-adzz (ss / i ent ent-type text-ss text-list attr-list block-text-data block-text-list block-count attr-count)
  (setq text-list '())
  (setq block-text-list '())
  (setq i 0)
  (setq block-count 0)
  (setq attr-count 0)
  
  ;; Loop through selection set
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    ;; Check if entity is TEXT, MTEXT, or INSERT (block)
    (cond
      ;; TEXT or MTEXT
      ((or (= ent-type "TEXT") (= ent-type "MTEXT"))
       (setq text-list (cons ent text-list))
      )
      
      ;; INSERT (block) - extract text content and attributes
      ((= ent-type "INSERT")
       (setq block-count (1+ block-count))
       
       ;; Extract text content from block definition (returns data, not entities)
       (setq block-text-data (extract-block-text-content-adzze ent))
       (if block-text-data
         (progn
           (setq attr-count (+ attr-count (length block-text-data)))
           ;; Add to data list instead of entity list
           (setq block-text-list (append block-text-data block-text-list))
         )
       )
       
       ;; Also extract attributes (these are real entities)
       (setq attr-list (extract-block-attributes-adzz ent))
       (if attr-list
         (progn
           (setq attr-count (+ attr-count (length attr-list)))
           (setq text-list (append attr-list text-list))
         )
       )
      )
    )
    
    (setq i (1+ i))
  )
  
  ;; Debug info
  (if (> block-count 0)
    (princ (strcat "\nFound " (itoa block-count) " block(s), extracted " (itoa attr-count) " text/attribute(s)"))
  )
  
  ;; Create new selection set with filtered text objects
  ;; Note: block text data is stored separately in global variable
  (setq *ADZZE_BLOCK_TEXT_DATA* block-text-list)
  
  (if text-list
    (progn
      (setq text-ss (ssadd))
      (foreach ent (reverse text-list)
        (ssadd ent text-ss)
      )
      text-ss
    )
    ;; If only block text (no real entities), return a dummy selection set marker
    (if block-text-list
      'block-text-only
      nil
    )
  )
)

;; Extract text content from block definition (return content with entity reference)
(defun extract-block-text-content-adzze (blk-ent / blk-name blk-def ent ent-type text-list blk-insert-pt blk-scale blk-rotation ent-data text-content vla-obj insert-pt scaled-pt rotated-pt final-pt)
  (setq text-list '())
  (setq blk-name (cdr (assoc 2 (entget blk-ent))))
  (setq blk-insert-pt (cdr (assoc 10 (entget blk-ent))))
  (setq blk-scale (cdr (assoc 41 (entget blk-ent))))
  (setq blk-rotation (cdr (assoc 50 (entget blk-ent))))
  
  (if (not blk-scale) (setq blk-scale 1.0))
  (if (not blk-rotation) (setq blk-rotation 0.0))
  
  ;; Get block definition
  (if (setq blk-def (tblsearch "BLOCK" blk-name))
    (progn
      ;; Get first entity in block definition
      (setq ent (cdr (assoc -2 blk-def)))
      
      ;; Loop through all entities in block
      (while ent
        (setq ent-type (cdr (assoc 0 (entget ent))))
        (setq ent-data (entget ent))
        
        ;; Check if it's TEXT or MTEXT
        (cond
          ((= ent-type "TEXT")
           (setq text-content (cdr (assoc 1 ent-data)))
           ;; Mark as block text with prefix
           (setq text-content (strcat "[BLOCK] " text-content))
           (setq insert-pt (cdr (assoc 10 ent-data)))
           ;; Transform point to world coordinates
           (setq final-pt (transform-point-adzze insert-pt blk-insert-pt blk-scale blk-rotation))
           ;; Store as (Y-coord block-text-entity text-content)
           ;; Use the actual entity in block definition
           (setq text-list (cons (list (caddr final-pt) ent text-content) text-list))
          )
          
          ((= ent-type "MTEXT")
           (setq vla-obj (vlax-ename->vla-object ent))
           (setq text-content (vla-get-TextString vla-obj))
           (setq text-content (cleanup-mtext-adzze text-content))
           (setq text-content (strcat "[BLOCK] " text-content))
           (setq insert-pt (cdr (assoc 10 ent-data)))
           (setq final-pt (transform-point-adzze insert-pt blk-insert-pt blk-scale blk-rotation))
           (setq text-list (cons (list (caddr final-pt) ent text-content) text-list))
          )
        )
        
        ;; Get next entity
        (setq ent (entnext ent))
        
        ;; Stop if we've left the block definition
        (if (and ent (= (cdr (assoc 0 (entget ent))) "ENDBLK"))
          (setq ent nil)
        )
      )
    )
  )
  
  text-list
)

;; Simple point transformation (approximate)
(defun transform-point-adzze (pt blk-pt scale rotation / x y new-x new-y)
  (setq x (car pt))
  (setq y (cadr pt))
  
  ;; Scale
  (setq x (* x scale))
  (setq y (* y scale))
  
  ;; Rotate (simplified - only for 0 rotation)
  ;; For full rotation support, need matrix transformation
  
  ;; Translate
  (setq new-x (+ x (car blk-pt)))
  (setq new-y (+ y (cadr blk-pt)))
  
  (list new-x new-y (+ y (cadr blk-pt)))
)

;; Extract text entities from block definition (OLD - not used)
(defun extract-block-text-adzz (blk-ent / blk-name blk-def ent ent-type text-list)
  (setq text-list '())
  (setq blk-name (cdr (assoc 2 (entget blk-ent))))
  
  ;; Get block definition
  (if (setq blk-def (tblsearch "BLOCK" blk-name))
    (progn
      ;; Get first entity in block definition
      (setq ent (cdr (assoc -2 blk-def)))
      
      ;; Loop through all entities in block
      (while ent
        (setq ent-type (cdr (assoc 0 (entget ent))))
        
        ;; Check if it's TEXT or MTEXT
        (if (or (= ent-type "TEXT") (= ent-type "MTEXT"))
          (setq text-list (cons ent text-list))
        )
        
        ;; Get next entity
        (setq ent (entnext ent))
        
        ;; Stop if we've left the block definition
        (if (and ent (= (cdr (assoc 0 (entget ent))) "ENDBLK"))
          (setq ent nil)
        )
      )
    )
  )
  
  (reverse text-list)
)
(defun extract-block-attributes-adzz (blk-ent / attr-list vla-obj has-attribs attribs i attr attr-ent)
  (setq attr-list '())
  
  ;; Method 1: Try using VLA object
  (if (setq vla-obj (vlax-ename->vla-object blk-ent))
    (progn
      (setq has-attribs (vlax-property-available-p vla-obj 'HasAttributes))
      
      (if (and has-attribs (= (vla-get-HasAttributes vla-obj) :vlax-true))
        (progn
          (setq attribs (vla-GetAttributes vla-obj))
          (setq i 0)
          (repeat (vlax-safearray-get-u-bound attribs 1)
            (setq attr (vlax-safearray-get-element attribs i))
            (setq attr-ent (vlax-vla-object->ename attr))
            (if attr-ent
              (setq attr-list (cons attr-ent attr-list))
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  
  ;; Method 2: Fallback - use entnext method
  (if (null attr-list)
    (progn
      (setq attr-ent (entnext blk-ent))
      (while (and attr-ent
                  (= "ATTRIB" (cdr (assoc 0 (entget attr-ent)))))
        (setq attr-list (cons attr-ent attr-list))
        (setq attr-ent (entnext attr-ent))
      )
    )
  )
  
  (reverse attr-list)
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
;;; ADZZR - Batch Modify Text Height
;;; Function: Filter TEXT/MTEXT in selected area, batch change height
;;; ─────────────────────────────────────────────────────────────

;@name 批量修改字高
;@group 文本编辑
;@desc 框选区域内，批量修改TEXT和MTEXT的字高，支持常用字高快选和自定义输入
;@require Selection
;@require ModelSpace

(defun c:ADZZR ()
  (setq ss nil)
  (setq text-ss nil)
  (setq text-count 0)

  (princ "\n=== ADZZR Batch Text Height Modifier ===")
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
         (setq height-result (show-height-dialog-adzzr))

         (cond
           ((and height-result (> (car height-result) 0))
            (apply-text-height-adzzr text-ss (car height-result) (cadr height-result))
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

(defun show-height-dialog-adzzr (/ dcl-file dcl-id result chosen-height scope-val custom-val)
  (setq dcl-file (vl-filename-mktemp nil nil ".dcl"))
  (setq chosen-height "0")   ;; 0 = custom
  (setq custom-val   "3.5")  ;; default custom input
  (setq scope-val    "0")    ;; 0=both 1=TEXT only 2=MTEXT only

  ;; Write DCL file
  (setq f (open dcl-file "w"))
  (write-line "adzzr_dlg : dialog {" f)
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
  (if (not (new_dialog "adzzr_dlg" dcl-id))
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

(defun apply-text-height-adzzr (ss new-height scope / i ent ent-type ent-data vla-obj cur-h)
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

(princ "\nCommand ADZZR loaded. Type ADZZR to start.")
(princ)


;@name 文字X轴对齐
;@group 文本编辑
;@desc 框选文字，以Y坐标最高的文字为基准，左/中/右对齐所有文字X坐标
;@require Selection
;@require ModelSpace
(defun c:ADZZF ()
  (setq ss nil)
  (setq text-ss nil)
  (setq text-count 0)
  (setq alignment nil)
  (setq top-ent nil)
  (setq ref-x nil)
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

           ;; ── Step 1: Apply text alignment attributes first ──────
           ;; Must run before reading insertion points for accuracy
           (apply-text-alignment-adzz text-ss alignment)
           (princ "\n[Step 1] Alignment attributes applied.")

           ;; ── Step 2: Find topmost text (highest Y) ─────────────
           (setq top-ent (find-topmost-adzzf text-ss))
           (setq ref-x   (get-align-x-adzzf top-ent alignment))
           (princ (strcat "\n[Step 2] Reference text found. Ref X = "
                          (rtos ref-x 2 4)))

           ;; ── Step 3: Move all texts to align X ──────────────────
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

;; ── Find topmost entity (highest Y of insertion point) ────────

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

;; ── Get reference X from a text entity based on alignment ─────
;; Left(0)  → TEXT uses point 10 X  / MTEXT uses point 10 X
;; Center(1)→ TEXT uses point 11 X  / MTEXT uses point 10 X
;; Right(2) → TEXT uses point 11 X  / MTEXT uses point 10 X

(defun get-align-x-adzzf (ent alignment / ent-type ent-data)
  (setq ent-type (cdr (assoc 0 (entget ent))))
  (setq ent-data (entget ent))
  (cond
    ((= ent-type "TEXT")
     (cond
       ((= alignment 0) (cadr (assoc 10 ent-data)))  ;; left  鈫?point 10
       (t               (cadr (assoc 11 ent-data)))  ;; center/right 鈫?point 11
     )
    )
    ((= ent-type "MTEXT")
     (cadr (assoc 10 ent-data))  ;; MTEXT attachment always at point 10
    )
  )
)

;; ── Move all texts so their alignment X equals target-x ───────

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



;@name 文字X轴对齐（仅移动）
;@group 文本编辑
;@desc 框选文字，以Y坐标最高的文字为基准对齐X坐标，不修改文字本身对齐属性
;@require Selection
;@require ModelSpace
(defun c:ADZZV ()
  (setq ss nil)
  (setq text-ss nil)
  (setq text-count 0)
  (setq alignment nil)
  (setq top-ent nil)
  (setq ref-x nil)
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

           ;; Step 1: Find topmost text (highest Y of point 10)
           (setq top-ent (find-topmost-adzzf text-ss))

           ;; Step 2: Get reference X from topmost text's ACTUAL visual edge
           ;; Uses textbox/BoundingBox — independent of alignment attribute
           (setq ref-x (get-visual-ref-x-adzzv top-ent alignment))

           (if ref-x
             (progn
               (princ (strcat "\n[Step 1] Topmost text ref X = " (rtos ref-x 2 4)))

               ;; Step 3: Move each text by delta so its visual edge = ref-x
               (move-texts-to-ref-adzzv text-ss ref-x alignment)
               (princ (strcat "\n[Step 2] " (itoa text-count)
                              " text(s) aligned. ADZZV complete!"))
             )
             (princ "\nError: Could not read bounding box of reference text.")
           )
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

;; ── Get actual visual Left / Center / Right X via bounding box ─
;; TEXT  → uses (textbox) which returns box relative to point 10
;; MTEXT → uses vla-GetBoundingBox for world-space box

(defun get-visual-ref-x-adzzv (ent alignment / ent-type ent-data bbox insert-x lx rx cx vla-obj minpt maxpt)
  (setq ent-type (cdr (assoc 0 (entget ent))))
  (setq ent-data (entget ent))

  (cond
    ((= ent-type "TEXT")
     (setq bbox     (textbox ent-data))           ;; ((x1 y1) (x2 y2)) relative to pt10
     (setq insert-x (cadr (assoc 10 ent-data)))
     (setq lx (+ insert-x (caar bbox)))           ;; actual left  edge X
     (setq rx (+ insert-x (car (cadr bbox))))     ;; actual right edge X
     (setq cx (/ (+ lx rx) 2.0))                  ;; actual center    X
     (cond
       ((= alignment 0) lx)
       ((= alignment 1) cx)
       ((= alignment 2) rx)
     )
    )

    ((= ent-type "MTEXT")
     (setq vla-obj (vlax-ename->vla-object ent))
     (vla-GetBoundingBox vla-obj 'minpt 'maxpt)
     (setq minpt (vlax-safearray->list minpt))
     (setq maxpt (vlax-safearray->list maxpt))
     (setq lx (car minpt))
     (setq rx (car maxpt))
     (setq cx (/ (+ lx rx) 2.0))
     (cond
       ((= alignment 0) lx)
       ((= alignment 1) cx)
       ((= alignment 2) rx)
     )
    )
  )
)

;; ── Move all texts so their visual edge aligns to ref-x ────────
;; Shift ALL points (10 and 11) by the same delta
;; This preserves the text's own alignment attribute completely

(defun move-texts-to-ref-adzzv (ss ref-x alignment / i ent cur-x delta)
  (setq i 0)
  (repeat (sslength ss)
    (setq ent   (ssname ss i))
    (setq cur-x (get-visual-ref-x-adzzv ent alignment))

    (if cur-x
      (progn
        (setq delta (- ref-x cur-x))
        ;; Only move if delta is meaningful (avoid float noise)
        (if (not (equal delta 0.0 0.001))
          (shift-text-x-adzzv ent delta)
        )
      )
      (princ (strcat "\n  [SKIP] Could not read bounding box for entity " (itoa i)))
    )

    (setq i (1+ i))
  )
)

;; ── Shift a single text entity's X by delta (move all points) ──

(defun shift-text-x-adzzv (ent delta / ent-type ent-data pt10 pt11)
  (setq ent-type (cdr (assoc 0 (entget ent))))
  (setq ent-data (entget ent))

  (cond
    ((= ent-type "TEXT")
     ;; Shift point 10
     (setq pt10 (assoc 10 ent-data))
     (setq ent-data
       (subst (list 10 (+ (cadr pt10) delta) (caddr pt10) (cadddr pt10))
              pt10 ent-data))
     ;; Shift point 11 if present (alignment/fit point)
     (setq pt11 (assoc 11 ent-data))
     (if pt11
       (setq ent-data
         (subst (list 11 (+ (cadr pt11) delta) (caddr pt11) (cadddr pt11))
                pt11 ent-data))
     )
     (entmod ent-data)
     (entupd ent)
    )

    ((= ent-type "MTEXT")
     ;; MTEXT only has point 10 as anchor
     (setq pt10 (assoc 10 ent-data))
     (setq ent-data
       (subst (list 10 (+ (cadr pt10) delta) (caddr pt10) (cadddr pt10))
              pt10 ent-data))
     (entmod ent-data)
     (entupd ent)
    )
  )
)

(princ "\nCommand ADZZV loaded. Type ADZZV to start.")
(princ)


;;; ─────────────────────────────────────────────────────────────
;;; ADZZE - Extract and Edit Text Content (DCL Native)
;;; Function: Extract text and edit in DCL dialog with list view
;;; v2: Common phrases organized into named category tabs
;;; ─────────────────────────────────────────────────────────────

;@name 提取并编辑文字
;@group 文本编辑
;@desc 提取文字到DCL对话框，逐个编辑后写回图纸；候选词支持分类标签管理
;@require Selection
;@require ModelSpace

;; ── Global dialog state (shared between action_tile callbacks) ──
(setq *ADZZE_CATS*        nil)  ;; ((cat-name phrase ...) ...)  – all categories
(setq *ADZZE_CUR_CAT*     0  )  ;; currently selected category index
(setq *ADZZE_CUR_PHRASES* nil)  ;; phrases of the selected category
(setq *ADZZE_TEXT_LINES*  nil)  ;; text lines being edited in the dialog
(setq *ADZZE_CUR_IDX*     0  )  ;; selected row in text_list
(setq *ADZZE_COMMON_IDX*  0  )  ;; selected row in common_list

;; ── Helper: return only the category names ────────────────────
(defun adzze-get-cat-names ()
  (mapcar 'car *ADZZE_CATS*)
)

;; ── Helper: overwrite phrases of one category and persist ─────
(defun adzze-update-cat-phrases (cat-idx new-phrases)
  (setq *ADZZE_CATS*
    (subst-nth cat-idx
      (cons (car (nth cat-idx *ADZZE_CATS*)) new-phrases)
      *ADZZE_CATS*))
  (save-common-phrases-adzze *ADZZE_CATS*)
)

(defun c:ADZZE ()
  (setq ss nil)
  (setq text-ss nil)
  (setq text-count 0)
  
  (princ "\n=== ADZZE Extract and Edit Text (DCL Native) ===")
  (princ "\nSelect text objects to extract and edit: ")
  (setq ss (ssget))
  
  (cond
    (ss
     (setq text-ss (filter-text-objects-adzz ss))
     
     (cond
       ((not text-ss)
        (princ "\nNo TEXT or MTEXT objects found in selection!")
       )
       
       (t
        ;; Handle both real entities and block text data
        (if (equal text-ss 'block-text-only)
          (progn
            ;; Only block text, no real entities
            (setq text-count (length *ADZZE_BLOCK_TEXT_DATA*))
            (princ (strcat "\nExtracted " (itoa text-count) " text object(s) from blocks"))
            
            ;; Use block text data directly
            (setq text-data *ADZZE_BLOCK_TEXT_DATA*)
          )
          (progn
            ;; Real entities (may also have block text)
            (setq text-count (sslength text-ss))
            (if *ADZZE_BLOCK_TEXT_DATA*
              (setq text-count (+ text-count (length *ADZZE_BLOCK_TEXT_DATA*)))
            )
            (princ (strcat "\nFiltered " (itoa text-count) " text object(s)"))
            
            ;; Extract text with entities (sorted by Y position)
            (setq text-data (extract-text-with-entities-adzze text-ss))
            
            ;; Merge with block text data
            (if *ADZZE_BLOCK_TEXT_DATA*
              (setq text-data (append text-data *ADZZE_BLOCK_TEXT_DATA*))
            )
            
            ;; Re-sort by Y coordinate
            (setq text-data (vl-sort text-data '(lambda (a b) (> (car a) (car b)))))
          )
        )
        
        (if text-data
          (progn
            ;; Auto copy to clipboard (silent)
            (setq text-lines (mapcar 'caddr text-data))
            (auto-copy-to-clipboard-adzze text-lines)
            (princ "\n[Text copied to clipboard]")
            
            ;; Show DCL editor
            (setq modified-data (show-text-editor-dcl-adzze text-data))
            
            (if modified-data
              (progn
                (setq modified-count (apply-modified-text-adzze modified-data))
                (princ (strcat "\nSuccess! Modified " (itoa modified-count) " text object(s)"))
                ;; Regenerate after dialog closes (if block text was modified)
                (if (check-block-text-modified-adzze modified-data)
                  (command "_.REGEN")
                )
              )
              (princ "\nNo changes made.")
            )
          )
          (princ "\nNo text content found!")
        )
       )
     )
    )
    (t (princ "\nNo objects selected."))
  )
  
  (princ)
)

;; ── Auto copy text to clipboard (silent, no error handling) ──

(defun auto-copy-to-clipboard-adzze (text-lines / all-text data-obj window-obj)
  ;; Combine all lines with newline
  (setq all-text "")
  (foreach line text-lines
    (setq all-text (strcat all-text line "\n"))
  )
  
  ;; Try using COM object to access clipboard (silent - ignore errors)
  (vl-catch-all-apply
    '(lambda ()
       (if (and (setq data-obj (vlax-create-object "htmlfile"))
                (setq window-obj (vlax-get-property data-obj 'parentWindow)))
         (progn
           (vlax-invoke-method 
             (vlax-get-property window-obj 'clipboardData) 
             'setData 
             "text" 
             all-text)
           (vlax-release-object data-obj)
         )
       )
     )
  )
  
  ;; Always return without error
  t
)

;; ── Extract text with entity references ──────────────────────

(defun extract-text-with-entities-adzze (ss / i ent text-list sorted-list)
  (setq text-list '())
  (setq i 0)
  
  ;; Collect: (Y-coord entity text-content)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq item (get-text-with-entity-adzze ent))
    
    (if item
      (setq text-list (cons item text-list))
    )
    
    (setq i (1+ i))
  )
  
  ;; Sort by Y (descending - top to bottom)
  (setq sorted-list (vl-sort text-list '(lambda (a b) (> (car a) (car b)))))
  
  sorted-list
)

;; ── Get text with entity reference ───────────────────────────

(defun get-text-with-entity-adzze (ent / ent-type ent-data text-content y-coord vla-obj insert-pt tag-name)
  (setq ent-type (cdr (assoc 0 (entget ent))))
  (setq ent-data (entget ent))
  (setq text-content "")
  (setq y-coord 0.0)
  
  (cond
    ;; TEXT object
    ((= ent-type "TEXT")
     (setq text-content (cdr (assoc 1 ent-data)))
     (setq insert-pt (cdr (assoc 10 ent-data)))
     (setq y-coord (caddr insert-pt))
    )
    
    ;; MTEXT object
    ((= ent-type "MTEXT")
     (setq vla-obj (vlax-ename->vla-object ent))
     (setq text-content (vla-get-TextString vla-obj))
     (setq text-content (cleanup-mtext-adzze text-content))
     (setq insert-pt (cdr (assoc 10 ent-data)))
     (setq y-coord (caddr insert-pt))
    )
    
    ;; ATTRIB object (block attribute)
    ((= ent-type "ATTRIB")
     (setq text-content (cdr (assoc 1 ent-data)))
     (setq tag-name (cdr (assoc 2 ent-data)))
     ;; Add tag name as prefix for clarity
     (setq text-content (strcat "[" tag-name "] " text-content))
     (setq insert-pt (cdr (assoc 10 ent-data)))
     (setq y-coord (caddr insert-pt))
    )
  )
  
  (if (and text-content (> (strlen text-content) 0))
    (list y-coord ent text-content)
    nil
  )
)

;; ── DCL dialog: text editor (left) + category/phrase panel (right) ─

(defun show-text-editor-dcl-adzze (text-data / dcl-file dcl-id result old-dynmode)

  ;; ── Initialise global dialog state ──────────────────────────
  (setq *ADZZE_CATS*        (load-common-phrases-adzze))
  (setq *ADZZE_CUR_CAT*     0)
  (setq *ADZZE_CUR_PHRASES* (if *ADZZE_CATS* (cdr (car *ADZZE_CATS*)) nil))
  (setq *ADZZE_TEXT_LINES*  (mapcar 'caddr text-data))
  (setq *ADZZE_CUR_IDX*     0)
  (setq *ADZZE_COMMON_IDX*  0)

  ;; Disable dynamic input to prevent tooltip freeze
  (setq old-dynmode (getvar "DYNMODE"))
  (setvar "DYNMODE" 0)

  ;; ── Write DCL file ───────────────────────────────────────────
  (setq dcl-file (vl-filename-mktemp nil nil ".dcl"))
  (setq f (open dcl-file "w"))

  (write-line "adzze_edit : dialog {" f)
  (write-line "  label = \"ADZZE - Edit Text  |  候选词分类管理\";" f)

  (write-line "  : row {" f)

  ;; ── LEFT COLUMN: text list + editor ─────────────────────────
  (write-line "    : column {" f)
  (write-line "      : text { label = \"文字列表（从上到下）/ Lines (top→bottom):\"; }" f)
  (write-line "      : list_box {" f)
  (write-line "        key = \"text_list\";" f)
  (write-line "        width = 40;" f)
  (write-line "        height = 15;" f)
  (write-line "        fixed_width_font = true;" f)
  (write-line "        multiple_select = false;" f)
  (write-line "      }" f)
  (write-line "      : text { label = \"编辑选中行 / Edit selected line:\"; }" f)
  (write-line "      : edit_box {" f)
  (write-line "        key = \"edit_text\";" f)
  (write-line "        edit_width = 40;" f)
  (write-line "        edit_limit = 2000;" f)
  (write-line "      }" f)
  (write-line "    }" f)   ;; end left column

  ;; ── MIDDLE COLUMN: phrase list ───────────────────────────────
  (write-line "    : column {" f)
  (write-line "      : text { label = \"候选文字（双击使用）/ Phrases (dbl-click):\"; }" f)
  (write-line "      : list_box {" f)
  (write-line "        key = \"common_list\";" f)
  (write-line "        width = 25;" f)
  (write-line "        height = 15;" f)
  (write-line "        fixed_width_font = true;" f)
  (write-line "        multiple_select = false;" f)
  (write-line "      }" f)
  ;; Phrase management row
  (write-line "      : row {" f)
  (write-line "        : edit_box { key = \"new_phrase\"; edit_width = 12; }" f)
  (write-line "        : button { key = \"add_phrase\"; label = \"添加\"; fixed_width = true; width = 6; }" f)
  (write-line "        : button { key = \"del_phrase\"; label = \"删除\"; fixed_width = true; width = 6; }" f)
  (write-line "      }" f)
  (write-line "    }" f)   ;; end middle column

  ;; ── RIGHT COLUMN: category tabs ──────────────────────────────
  (write-line "    : column {" f)
  (write-line "      : text { label = \"分类标签 / Categories:\"; }" f)
  (write-line "      : list_box {" f)
  (write-line "        key = \"cat_list\";" f)
  (write-line "        width = 25;" f)
  (write-line "        height = 15;" f)
  (write-line "        fixed_width_font = true;" f)
  (write-line "        multiple_select = false;" f)
  (write-line "      }" f)
  ;; Category management row
  (write-line "      : row {" f)
  (write-line "        : edit_box { key = \"new_cat\"; edit_width = 12; }" f)
  (write-line "        : button { key = \"add_cat\"; label = \"新增\"; fixed_width = true; width = 6; }" f)
  (write-line "        : button { key = \"del_cat\"; label = \"删除\"; fixed_width = true; width = 6; }" f)
  (write-line "      }" f)
  (write-line "    }" f)   ;; end right column

  (write-line "  }" f)     ;; end row

  ;; Bottom action buttons
  (write-line "  : row {" f)
  (write-line "    : button { key = \"update\"; label = \"更新当前行 Update\"; fixed_width = true; }" f)
  (write-line "    : button { key = \"apply\"; label = \"应用全部 Apply All\"; is_default = true; fixed_width = true; }" f)
  (write-line "    : button { key = \"cancel\"; label = \"取消 Cancel\"; is_cancel = true; fixed_width = true; }" f)
  (write-line "  }" f)

  (write-line "}" f)
  (close f)

  ;; ── Load and initialise dialog ───────────────────────────────
  (setq dcl-id (load_dialog dcl-file))

  (if (not (new_dialog "adzze_edit" dcl-id))
    (progn
      (princ "\nError: Cannot load ADZZE dialog.")
      (setq result nil)
    )
    (progn

      ;; Populate text list
      (start_list "text_list")
      (mapcar 'add_list *ADZZE_TEXT_LINES*)
      (end_list)
      (set_tile "text_list" "0")
      (set_tile "edit_text" (nth 0 *ADZZE_TEXT_LINES*))

      ;; Populate category list
      (start_list "cat_list")
      (if *ADZZE_CATS* (mapcar 'add_list (adzze-get-cat-names)))
      (end_list)
      (if *ADZZE_CATS* (set_tile "cat_list" "0"))

      ;; Populate phrase list for first category
      (start_list "common_list")
      (if *ADZZE_CUR_PHRASES* (mapcar 'add_list *ADZZE_CUR_PHRASES*))
      (end_list)

      ;; ── ACTION: switch category (acts like clicking a tab) ────
      (action_tile "cat_list"
        "(progn
           (setq *ADZZE_CUR_CAT* (atoi $value))
           (setq *ADZZE_CUR_PHRASES*
             (if *ADZZE_CATS* (cdr (nth *ADZZE_CUR_CAT* *ADZZE_CATS*)) nil))
           (setq *ADZZE_COMMON_IDX* 0)
           (start_list \"common_list\")
           (if *ADZZE_CUR_PHRASES* (mapcar 'add_list *ADZZE_CUR_PHRASES*))
           (end_list)
         )")

      ;; ── ACTION: add new category ──────────────────────────────
      (action_tile "add_cat"
        "(progn
           (setq *adzze-nc* (get_tile \"new_cat\"))
           (if (and *adzze-nc* (> (strlen *adzze-nc*) 0))
             (progn
               (setq *ADZZE_CATS* (append *ADZZE_CATS* (list (list *adzze-nc*))))
               (save-common-phrases-adzze *ADZZE_CATS*)
               (set_tile \"new_cat\" \"\")
               (setq *ADZZE_CUR_CAT* (1- (length *ADZZE_CATS*)))
               (setq *ADZZE_CUR_PHRASES* nil)
               (start_list \"cat_list\")
               (mapcar 'add_list (adzze-get-cat-names))
               (end_list)
               (set_tile \"cat_list\" (itoa *ADZZE_CUR_CAT*))
               (start_list \"common_list\")
               (end_list)
             )
           )
         )")

      ;; ── ACTION: delete selected category ─────────────────────
      (action_tile "del_cat"
        "(progn
           (if (and *ADZZE_CATS* (>= *ADZZE_CUR_CAT* 0) (< *ADZZE_CUR_CAT* (length *ADZZE_CATS*)))
             (progn
               ;; Check if category has phrases
               (setq *adzze-cat-phrases* (cdr (nth *ADZZE_CUR_CAT* *ADZZE_CATS*)))
               (if (and *adzze-cat-phrases* (> (length *adzze-cat-phrases*) 0))
                 ;; Category has phrases - prevent deletion
                 (alert \"无法删除此分类！\\n该分类下还有候选文字。\\n请先删除所有候选文字后再删除分类。\\n\\nCannot delete this category!\\nIt still contains phrases.\\nPlease delete all phrases first.\")
                 ;; Category is empty - allow deletion
                 (progn
                   (setq *ADZZE_CATS* (remove-nth *ADZZE_CUR_CAT* *ADZZE_CATS*))
                   (save-common-phrases-adzze *ADZZE_CATS*)
                   (if (>= *ADZZE_CUR_CAT* (length *ADZZE_CATS*))
                     (setq *ADZZE_CUR_CAT* (max 0 (1- (length *ADZZE_CATS*))))
                   )
                   (setq *ADZZE_CUR_PHRASES*
                     (if *ADZZE_CATS* (cdr (nth *ADZZE_CUR_CAT* *ADZZE_CATS*)) nil))
                   (setq *ADZZE_COMMON_IDX* 0)
                   (start_list \"cat_list\")
                   (if *ADZZE_CATS* (mapcar 'add_list (adzze-get-cat-names)))
                   (end_list)
                   (if *ADZZE_CATS* (set_tile \"cat_list\" (itoa *ADZZE_CUR_CAT*)))
                   (start_list \"common_list\")
                   (if *ADZZE_CUR_PHRASES* (mapcar 'add_list *ADZZE_CUR_PHRASES*))
                   (end_list)
                 )
               )
             )
           )
         )")

      ;; ── ACTION: phrase list – double-click inserts into editor ─
      (action_tile "common_list"
        "(progn
           (setq *ADZZE_COMMON_IDX* (atoi $value))
           (if (= $reason 4)
             (progn
               (setq *adzze-p* (nth *ADZZE_COMMON_IDX* *ADZZE_CUR_PHRASES*))
               (if *adzze-p*
                 (progn
                   (set_tile \"edit_text\" *adzze-p*)
                   (setq *ADZZE_TEXT_LINES*
                     (subst-nth *ADZZE_CUR_IDX* *adzze-p* *ADZZE_TEXT_LINES*))
                   (start_list \"text_list\")
                   (mapcar 'add_list *ADZZE_TEXT_LINES*)
                   (end_list)
                   ;; Move to next line if not at the end
                   (if (< *ADZZE_CUR_IDX* (1- (length *ADZZE_TEXT_LINES*)))
                     (setq *ADZZE_CUR_IDX* (1+ *ADZZE_CUR_IDX*))
                   )
                   (set_tile \"text_list\" (itoa *ADZZE_CUR_IDX*))
                   (set_tile \"edit_text\" (nth *ADZZE_CUR_IDX* *ADZZE_TEXT_LINES*))
                   ;; Set focus to text_list
                   (mode_tile \"text_list\" 2)
                 )
               )
             )
           )
         )")

      ;; ── ACTION: add phrase to current category ────────────────
      (action_tile "add_phrase"
        "(progn
           (setq *adzze-np* (get_tile \"new_phrase\"))
           (if (and *adzze-np* (> (strlen *adzze-np*) 0) *ADZZE_CATS*)
             (progn
               (setq *ADZZE_CUR_PHRASES*
                 (append *ADZZE_CUR_PHRASES* (list *adzze-np*)))
               (adzze-update-cat-phrases *ADZZE_CUR_CAT* *ADZZE_CUR_PHRASES*)
               (set_tile \"new_phrase\" \"\")
               (start_list \"common_list\")
               (mapcar 'add_list *ADZZE_CUR_PHRASES*)
               (end_list)
             )
           )
         )")

      ;; ── ACTION: delete selected phrase ────────────────────────
      (action_tile "del_phrase"
        "(progn
           (if (and *ADZZE_CUR_PHRASES*
                    (>= *ADZZE_COMMON_IDX* 0)
                    (< *ADZZE_COMMON_IDX* (length *ADZZE_CUR_PHRASES*)))
             (progn
               (setq *ADZZE_CUR_PHRASES*
                 (remove-nth *ADZZE_COMMON_IDX* *ADZZE_CUR_PHRASES*))
               (adzze-update-cat-phrases *ADZZE_CUR_CAT* *ADZZE_CUR_PHRASES*)
               (if (>= *ADZZE_COMMON_IDX* (length *ADZZE_CUR_PHRASES*))
                 (setq *ADZZE_COMMON_IDX* (max 0 (1- (length *ADZZE_CUR_PHRASES*))))
               )
               (start_list \"common_list\")
               (if *ADZZE_CUR_PHRASES* (mapcar 'add_list *ADZZE_CUR_PHRASES*))
               (end_list)
               (if *ADZZE_CUR_PHRASES*
                 (set_tile \"common_list\" (itoa *ADZZE_COMMON_IDX*)))
             )
           )
         )")

      ;; ── ACTION: select line in text list (auto-save current) ──
      (action_tile "text_list"
        "(progn
           (setq *adzze-ev* (get_tile \"edit_text\"))
           (if (not (equal *adzze-ev* (nth *ADZZE_CUR_IDX* *ADZZE_TEXT_LINES*)))
             (setq *ADZZE_TEXT_LINES*
               (subst-nth *ADZZE_CUR_IDX* *adzze-ev* *ADZZE_TEXT_LINES*))
           )
           (setq *ADZZE_CUR_IDX* (atoi $value))
           (set_tile \"edit_text\" (nth *ADZZE_CUR_IDX* *ADZZE_TEXT_LINES*))
           (start_list \"text_list\")
           (mapcar 'add_list *ADZZE_TEXT_LINES*)
           (end_list)
           (set_tile \"text_list\" (itoa *ADZZE_CUR_IDX*))
         )")

      ;; ── ACTION: update current line ───────────────────────────
      (action_tile "update"
        "(progn
           (setq *adzze-ev* (get_tile \"edit_text\"))
           (setq *ADZZE_TEXT_LINES*
             (subst-nth *ADZZE_CUR_IDX* *adzze-ev* *ADZZE_TEXT_LINES*))
           (start_list \"text_list\")
           (mapcar 'add_list *ADZZE_TEXT_LINES*)
           (end_list)
           (set_tile \"text_list\" (itoa *ADZZE_CUR_IDX*))
         )")

      ;; ── ACTION: apply all & close ─────────────────────────────
      (action_tile "apply"
        "(progn
           (setq *adzze-ev* (get_tile \"edit_text\"))
           (setq *ADZZE_TEXT_LINES*
             (subst-nth *ADZZE_CUR_IDX* *adzze-ev* *ADZZE_TEXT_LINES*))
           (done_dialog 1)
         )")

      (action_tile "cancel" "(done_dialog 0)")

      (setq result (start_dialog))
      (unload_dialog dcl-id)
    )
  )

  ;; Cleanup
  (vl-file-delete dcl-file)
  (setvar "DYNMODE" old-dynmode)

  ;; Return (entity . new-text) pairs if user clicked Apply
  (if (= result 1)
    (mapcar '(lambda (item line) (list (cadr item) line))
            text-data *ADZZE_TEXT_LINES*)
    nil
  )
)

;; ── Copy text to clipboard ───────────────────────────────────

(defun copy-to-clipboard-adzze (text-lines / all-text vbs-file)
  ;; Combine all lines with newline
  (setq all-text "")
  (foreach line text-lines
    (setq all-text (strcat all-text line "\n"))
  )
  
  ;; Escape quotes for VBScript
  (while (vl-string-search "\"" all-text)
    (setq all-text (vl-string-subst "\"\"" "\"" all-text))
  )
  
  ;; Create VBScript to copy to clipboard
  (setq vbs-file (strcat (getenv "TEMP") "\\adzze_copy.vbs"))
  
  (setq f (open vbs-file "w"))
  (write-line "Set objHTML = CreateObject(\"htmlfile\")" f)
  (write-line "Set objWindow = objHTML.parentWindow" f)
  (write-line (strcat "strText = \"" all-text "\"") f)
  (write-line "objWindow.clipboardData.SetData \"text\", strText" f)
  (close f)
  
  ;; Execute VBScript
  (startapp "wscript.exe" (strcat "\"" vbs-file "\""))
  
  ;; Note: VBS file will remain, but it's small and in temp folder
  
  t
)

;; ── Helper: substitute nth element in list ───────────────────

(defun subst-nth (n new-val lst / i result)
  (setq i 0)
  (setq result '())
  (foreach item lst
    (if (= i n)
      (setq result (append result (list new-val)))
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)

;; ── Helper: remove nth element from list ─────────────────────

(defun remove-nth (n lst / i result)
  (setq i 0)
  (setq result '())
  (foreach item lst
    (if (/= i n)
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)

;; ── Load common phrases with categories ──────────────────────
;; Config file format:
;;   [CategoryName]
;;   phrase1
;;   phrase2
;;   [AnotherCategory]
;;   ...
;; Returns: ((cat-name phrase1 phrase2 ...) ...)

(defun load-common-phrases-adzze (/ config-file cats f line cur-cat cur-phrases)
  (setq config-file (strcat (getenv "APPDATA") "\\ADZZE_CommonPhrases.txt"))
  (setq cats       '())
  (setq cur-cat    nil)
  (setq cur-phrases '())

  (if (findfile config-file)
    (progn
      (setq f (open config-file "r"))
      (if f
        (progn
          (while (setq line (read-line f))
            (cond
              ;; Category header line: [CategoryName]
              ((and (> (strlen line) 2)
                    (= (substr line 1 1) "[")
                    (vl-string-search "]" line))
               ;; Save previous category before starting a new one
               (if cur-cat
                 (setq cats (append cats
                   (list (cons cur-cat (reverse cur-phrases)))))
               )
               ;; Extract name between [ and ]
               (setq cur-cat
                 (substr line 2 (- (strlen line) 2)))
               (setq cur-phrases '())
              )
              ;; Phrase line (non-empty)
              ((and cur-cat (> (strlen line) 0))
               (setq cur-phrases (cons line cur-phrases))
              )
            )
          )
          ;; Save last category
          (if cur-cat
            (setq cats (append cats
              (list (cons cur-cat (reverse cur-phrases)))))
          )
          (close f)
        )
      )
    )
  )

  ;; If nothing loaded, create sensible defaults
  (if (null cats)
    (progn
      (setq cats
        (list
          (list "常用"   "待定" "核实" "修改" "确认" "删除" "保留")
          (list "状态"   "已完成" "进行中" "未开始" "已取消")
          (list "审核"   "符合要求" "需要复核" "请设计师确认" "按图施工")
        ))
      (save-common-phrases-adzze cats)
    )
  )

  cats
)

;; ── Save common phrases with categories to config file ────────

(defun save-common-phrases-adzze (cats / config-file f)
  (setq config-file (strcat (getenv "APPDATA") "\\ADZZE_CommonPhrases.txt"))
  (setq f (open config-file "w"))
  (if f
    (progn
      (foreach cat-entry cats
        ;; Write [CategoryName]
        (write-line (strcat "[" (car cat-entry) "]") f)
        ;; Write each phrase
        (foreach phrase (cdr cat-entry)
          (write-line phrase f)
        )
      )
      (close f)
      t
    )
    nil
  )
)

;; ── Apply modified text to drawing ───────────────────────────

(defun apply-modified-text-adzze (modified-data / modified-count ent new-text ent-type ent-data vla-obj clean-text)
  (setq modified-count 0)
  
  (foreach item modified-data
    (setq ent (car item))
    (setq new-text (cadr item))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    (cond
      ;; TEXT object (including block definition text)
      ((= ent-type "TEXT")
       ;; Remove [BLOCK] prefix if present
       (if (= (substr new-text 1 7) "[BLOCK]")
         (setq new-text (substr new-text 9))
       )
       (setq ent-data (entget ent))
       (setq ent-data (subst (cons 1 new-text) (assoc 1 ent-data) ent-data))
       (entmod ent-data)
       (entupd ent)
       (setq modified-count (1+ modified-count))
      )
      
      ;; MTEXT object (including block definition mtext)
      ((= ent-type "MTEXT")
       ;; Remove [BLOCK] prefix if present
       (if (= (substr new-text 1 7) "[BLOCK]")
         (setq new-text (substr new-text 9))
       )
       (setq vla-obj (vlax-ename->vla-object ent))
       (vla-put-TextString vla-obj new-text)
       (entupd ent)
       (setq modified-count (1+ modified-count))
      )
      
      ;; ATTRIB object (block attribute)
      ((= ent-type "ATTRIB")
       ;; Remove tag prefix if present: "[TAG] value" -> "value"
       (setq clean-text new-text)
       (if (and (= (substr clean-text 1 1) "[")
                (setq close-pos (vl-string-search "]" clean-text)))
         (setq clean-text (substr clean-text (+ close-pos 3)))
       )
       (setq ent-data (entget ent))
       (setq ent-data (subst (cons 1 clean-text) (assoc 1 ent-data) ent-data))
       (entmod ent-data)
       (entupd ent)
       (setq modified-count (1+ modified-count))
      )
    )
  )
  
  modified-count
)

;; ── Check if block text was modified ─────────────────────────

(defun check-block-text-modified-adzze (modified-data / has-block-text)
  (setq has-block-text nil)
  
  (foreach item modified-data
    (setq new-text (cadr item))
    ;; Check if any text has [BLOCK] prefix
    (if (and (>= (strlen new-text) 7)
             (= (substr new-text 1 7) "[BLOCK]"))
      (setq has-block-text t)
    )
  )
  
  has-block-text
)

;; ── Clean up MTEXT formatting ────────────────────────────────

(defun cleanup-mtext-adzze (text / result)
  (setq result text)
  
  ;; Replace \P with newline
  (while (vl-string-search "\\P" result)
    (setq result (vl-string-subst "\n" "\\P" result))
  )
  
  (while (vl-string-search "\\p" result)
    (setq result (vl-string-subst "\n" "\\p" result))
  )
  
  result
)

(princ "\nCommand ADZZE loaded. Type ADZZE to extract and edit text.")
(princ)
