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
;;; ─────────────────────────────────────────────────────────────

;@name 提取并编辑文字
;@group 文本编辑
;@desc 提取文字到DCL对话框，逐个编辑后写回图纸（无编码问题）
;@require Selection
;@require ModelSpace

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
        (setq text-count (sslength text-ss))
        (princ (strcat "\nFiltered " (itoa text-count) " text object(s)"))
        
        ;; Extract text with entities (sorted by Y position)
        (setq text-data (extract-text-with-entities-adzze text-ss))
        
        (if text-data
          (progn
            ;; Show DCL editor
            (setq modified-data (show-text-editor-dcl-adzze text-data))
            
            (if modified-data
              (progn
                (setq modified-count (apply-modified-text-adzze modified-data))
                (princ (strcat "\nSuccess! Modified " (itoa modified-count) " text object(s)"))
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

(defun get-text-with-entity-adzze (ent / ent-type ent-data text-content y-coord vla-obj insert-pt)
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
  )
  
  (if (and text-content (> (strlen text-content) 0))
    (list y-coord ent text-content)
    nil
  )
)

;; ── Show DCL text editor ──────────────────────────────────────

(defun show-text-editor-dcl-adzze (text-data / dcl-file dcl-id result text-lines current-idx edit-value modified-flags)
  (setq dcl-file (vl-filename-mktemp nil nil ".dcl"))
  (setq text-lines (mapcar 'caddr text-data))  ;; Extract text content
  (setq modified-flags (mapcar '(lambda (x) nil) text-data))  ;; Track modifications
  (setq current-idx 0)
  
  ;; Create DCL file
  (setq f (open dcl-file "w"))
  (write-line "adzze_edit : dialog {" f)
  (write-line "  label = \"ADZZE - Edit Text Content\";" f)
  (write-line "  : text {" f)
  (write-line "    label = \"Select a line to edit (top to bottom order):\";" f)
  (write-line "  }" f)
  (write-line "  : list_box {" f)
  (write-line "    key = \"text_list\";" f)
  (write-line "    width = 80;" f)
  (write-line "    height = 15;" f)
  (write-line "    fixed_width_font = true;" f)
  (write-line "    multiple_select = false;" f)
  (write-line "  }" f)
  (write-line "  : text {" f)
  (write-line "    label = \"Edit selected line:\";" f)
  (write-line "  }" f)
  (write-line "  : edit_box {" f)
  (write-line "    key = \"edit_text\";" f)
  (write-line "    edit_width = 80;" f)
  (write-line "    edit_limit = 2000;" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : button {" f)
  (write-line "      key = \"update\";" f)
  (write-line "      label = \"Update Line\";" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"apply\";" f)
  (write-line "      label = \"Apply All Changes\";" f)
  (write-line "      is_default = true;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "    : button {" f)
  (write-line "      key = \"cancel\";" f)
  (write-line "      label = \"Cancel\";" f)
  (write-line "      is_cancel = true;" f)
  (write-line "      fixed_width = true;" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  
  ;; Load and show dialog
  (setq dcl-id (load_dialog dcl-file))
  
  (if (not (new_dialog "adzze_edit" dcl-id))
    (progn
      (princ "\nError: Cannot load dialog.")
      (setq result nil)
    )
    (progn
      ;; Populate list
      (start_list "text_list")
      (mapcar 'add_list text-lines)
      (end_list)
      
      ;; Set initial selection
      (set_tile "text_list" "0")
      (set_tile "edit_text" (nth 0 text-lines))
      
      ;; Action for list selection
      (action_tile "text_list"
        "(progn
           (setq current-idx (atoi $value))
           (set_tile \"edit_text\" (nth current-idx text-lines))
         )")
      
      ;; Action for Update button
      (action_tile "update"
        "(progn
           (setq edit-value (get_tile \"edit_text\"))
           (setq text-lines (subst-nth current-idx edit-value text-lines))
           (setq modified-flags (subst-nth current-idx t modified-flags))
           (start_list \"text_list\")
           (mapcar 'add_list text-lines)
           (end_list)
           (set_tile \"text_list\" (itoa current-idx))
         )")
      
      ;; Action for Apply button
      (action_tile "apply"
        "(progn
           (setq edit-value (get_tile \"edit_text\"))
           (setq text-lines (subst-nth current-idx edit-value text-lines))
           (setq modified-flags (subst-nth current-idx t modified-flags))
           (done_dialog 1)
         )")
      
      ;; Action for Cancel button
      (action_tile "cancel" "(done_dialog 0)")
      
      ;; Show dialog
      (setq result (start_dialog))
      (unload_dialog dcl-id)
    )
  )
  
  ;; Delete temporary DCL file
  (vl-file-delete dcl-file)
  
  ;; Return modified data if user clicked Apply
  (if (= result 1)
    (mapcar '(lambda (item line) (list (cadr item) line)) text-data text-lines)
    nil
  )
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

;; ── Apply modified text to drawing ───────────────────────────

(defun apply-modified-text-adzze (modified-data / modified-count ent new-text ent-type ent-data vla-obj)
  (setq modified-count 0)
  
  (foreach item modified-data
    (setq ent (car item))
    (setq new-text (cadr item))
    (setq ent-type (cdr (assoc 0 (entget ent))))
    
    (cond
      ;; TEXT object
      ((= ent-type "TEXT")
       (setq ent-data (entget ent))
       (setq ent-data (subst (cons 1 new-text) (assoc 1 ent-data) ent-data))
       (entmod ent-data)
       (entupd ent)
       (setq modified-count (1+ modified-count))
      )
      
      ;; MTEXT object
      ((= ent-type "MTEXT")
       (setq vla-obj (vlax-ename->vla-object ent))
       (vla-put-TextString vla-obj new-text)
       (entupd ent)
       (setq modified-count (1+ modified-count))
      )
    )
  )
  
  modified-count
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
