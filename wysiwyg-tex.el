;;; WYSIWYG-TeX.el --- Support WYSIWYG edit for TEX on Emacs

;; Copyright (c) 2011, Sho Nakatani [@laysakura on twitter]
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:

;;     * Redistributions of source code must retain the above copyright notice,
;;       this list of conditions and the following disclaimer.

;;     * Redistributions in binary form must reproduce the above copyright notice,
;;       this list of conditions and the following disclaimer in the documentation
;;       and/or other materials provided with the distribution.

;;     * Neither the name of Zend Technologies USA, Inc. nor the names of its
;;       contributors may be used to endorse or promote products derived from this
;;       software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; WYSIWYG-TeX.el supports your WYSIWYG editting of TEX file on Emacs.
;; WYSIWYG-TeX.el displays both:
;;     * A page cursor is on
;;     * Whole page typeset from TEX


;;; Installation:
;;
;; Add following settings on ~/.emacs
;;
;; (require 'wysiwyg-tex)
;; (add-hook 'tex-mode-hook   ; <mode-you-use-when-editting-tex>-hook
;;           '(lambda ()
;;              ;; Customizeable variables
;;              (setq wysiwyg-tex-tex2dvi-command "latex" ; Command to convert TEX into DVI. ("latex" by default)
;;                    wysiwyg-tex-using-color-package t ; Whether to always \usepackage{color}. (nil by default)
;;                    wysiwyg-tex-typeset-3-times t) ; Whether to repeat typesetting 3 times (t by default)
;;
;;              ;; key-binds for tex-mode
;;              (local-set-key "\C-c\C-p" 'wysiwyg-tex-show-preview) ; Displays a page around cursor.
;;              (local-set-key "\C-cp" 'wysiwyg-tex-show-whole-preview))) ; Displays the whole page.


;;; Usage:
;;
;; While visiting TEX buffer,
;;     * \C-c\C-p (wysiwyg-tex-show-preview) : Displays a page around cursor.
;;     * \C-cp (wysiwyg-tex-show-whole-preview) : Displays the whole page.
;;
;; If there is something wrong in TEX file, an error log appears instead of preview.


;;; Further Information:
;;
;; See: https://github.com/laysakura/WYSIWYG-TeX-el


;;; Code:

(require 'doc-view)

;;; Customizable variables
(defgroup wysiwyg-tex nil
  "WYSIWYG TeX"
  :group 'emacs)

(defcustom wysiwyg-tex-tex2dvi-command "latex"
  "Command to create DVI from TEX.
This variable is used like this:

    'wysiwyg-tex-tex2dvi-command'  TEX-FILE

Note that this emacs lisp doesn't care about command-line options.
If you need ones, make some shell script and apply it as 'wysiwyg-tex-tex2dvi-command'."
  :type '(string)
  :group 'wysiwyg-tex)

(defcustom wysiwyg-tex-typeset-3-times t
  "Specify whether to typeset TEX file 3 times with 'wysiwyg-tex-tex2dvi-command'.
To resolve \\label and \\ref dependency, and to make table of contents,
you need to typeset your TEX file at most 3 times.
If you know it and use some special command like 'latexmk', you can set 'nil'.
Otherwise, leave this 't'"
  :type '(boolean)
  :group 'wysiwyg-tex)

(defcustom wysiwyg-tex-dvi2ps-command "dvips"
  "Command to create PS from DVI.
This variable is used like this:

    'wysiwyg-tex-dvi2ps-command'  DVI-FILE

Note that this emacs lisp doesn't care about command-line options.
If you need ones, make some shell script and apply it as 'wysiwyg-tex-dvi2ps-command'."
  :type '(string)
  :group 'wysiwyg-tex)

(defcustom wysiwyg-tex-extract-ps-page-command "psselect"
  "Command to extract pages from PS.
This variable is used like this:

    'wysiwyg-tex-extract-ps-page-command'  PAGE-NUM  PS-FILE  EXTRACTED-PS-FILE

where PAGE-NUM is single integer which specify a page."
  :type '(string)
  :group 'wysiwyg-tex)


(defcustom wysiwyg-tex-using-color-package t
  "If you always \\usepackage{color}, set 't'.
Otherwise, set 'nil'.

How is this variable used:
If this variable is 't', this emacs lisp inserts (NOT in your TEX file :-D ) a marker.
More precisely, it inserts

    \\mbox{\\textcolor{white}{.,.,.}\\hspace{-5.0ex}}

So, in order to set the marker color white, you need to \\usepackage{color}.
If this variable is 'nil', this emacs lisp uses a marker below:

    \\mbox{.,.,.\\hspace{-5.0ex}}

This marker might annoy you, so I recommend you to always \\usepackage{color}
and set this variable 't'."
  :type '(boolean)
  :group 'wysiwyg-tex)

(defcustom wysiwyg-tex-using-color-package nil
  "If you always \\usepackage{color}, set 't'.
Otherwise, set 'nil'.

How is this variable used:
If this variable is 't', this emacs lisp inserts (NOT in your TEX file :-D ) a marker.
More precisely, it inserts

    \\mbox{\\textcolor{white}{.,.,.}\\hspace{-5.0ex}}

So, in order to set the marker color white, you need to \\usepackage{color}.
If this variable is 'nil', this emacs lisp uses a marker below:

    \\mbox{.,.,.\\hspace{-5.0ex}}

This marker might annoy you, so I recommend you to always \\usepackage{color}
and set this variable 't'."
  :type '(boolean)
  :group 'wysiwyg-tex)

(defcustom wysiwyg-tex-marker-width "5.0ex"
  "Specify marker width.
This variable is used like this:

<When wysiwyg-tex-using-color-package is 't'>
    \\mbox{\\textcolor{white}{.,.,.}\\hspace{-'wysiwyg-tex-marker-width'}}

<When wysiwyg-tex-using-color-package is 'nil'>
    \\mbox{.,.,.\\hspace{-'wysiwyg-tex-marker-width'}}

Normally, you don't need to change this variable."
  :type '(string)
  :group 'wysiwyg-tex)




;;; Constants
(defun wysiwyg-tex-file-prefix ()
  "wysiwyg-tex-files")

(defun wysiwyg-tex-tmp-tex-name ()
  (concat (wysiwyg-tex-file-prefix) ".tex"))
(defun wysiwyg-tex-tmp-sub-tex-name ()
  (concat (wysiwyg-tex-file-prefix) "-sub.tex"))
(defun wysiwyg-tex-tmp-dvi-name ()
  (concat (wysiwyg-tex-file-prefix) ".dvi"))
(defun wysiwyg-tex-whole-ps-name ()
  (concat (wysiwyg-tex-file-prefix) ".ps"))
(defun wysiwyg-tex-extracted-ps-name ()
  (concat (wysiwyg-tex-file-prefix) "-extracted.ps"))
(defun wysiwyg-tex-main-src-reminder-name ()
  (concat (wysiwyg-tex-file-prefix) "-main-src"))



(defun wysiwyg-tex-marker ()
  ".,.,.")

(defun wysiwyg-tex-marker-to-insert-in-tex ()
  (if wysiwyg-tex-using-color-package
      (concat "\\mbox{{\\tiny \\textcolor{white}{" (wysiwyg-tex-marker) "}\\hspace{-" wysiwyg-tex-marker-width "}}}")
    (concat "\\mbox{{\\tiny " (wysiwyg-tex-marker) "\\hspace{-" wysiwyg-tex-marker-width "}}}")))

(defun wysiwyg-tex-typesetting-log-buffer-name ()
  "*WYSIWYG TeX - Last typesetting*")

(defun wysiwyg-tex-dvi2ps-log-buffer-name ()
  "*WYSIWYG TeX - Last converting DVI into PS*")

(defun wysiwyg-tex-extract-ps-page-log-buffer-name ()
  "*WYSIWYG TeX - Last extracting page from PS*")




;;; File utilities
(defun wysiwyg-tex-extract-file-name-from-path (path)
  "Returns file name of path.
If path is not a full path, just returns path."
  (if (string-match ".*\\/\\(.*\\)" path)
      (match-string 1 path)
    path))

(defun wysiwyg-tex-replace-all-in (filepath from-str to-str)
  (save-current-buffer
    (save-excursion
      (set-buffer (find-file-noselect filepath))
      (goto-char (point-min))
      (while (search-forward from-str nil t)
        (replace-match to-str nil t))
      (save-buffer)
      (kill-buffer))))




;;; Buffer control utilities
(defun wysiwyg-tex-buffer-exists-p (buf-name)
  "Returns if buffer with 'buf-name' exists."
  (not (eq (get-buffer buf-name) nil)))
;; (wysiwyg-tex-buffer-exists-p "*scratch*")

(defun wysiwyg-tex-kill-buffer-if-exists (buf-name)
  "Kills buffer with 'buf-name' if exists."
  (if (wysiwyg-tex-buffer-exists-p buf-name)
      (kill-buffer buf-name)))
;; (wysiwyg-tex-kill-buffer-if-exists "wysiwyg-tex-preview-extracted.ps")

(defun wysiwyg-tex-erase-buffer (buf-name)
  "Erases the whole contents of buffer with 'buf-name' if exists."
  (if (wysiwyg-tex-buffer-exists-p buf-name)
      (save-current-buffer
        (set-buffer buf-name)
        (erase-buffer))))
;; (wysiwyg-tex-erase-buffer "*scratch*")


;;; Logs
(defun wysiwyg-tex-display-buffer-with-header (buf-name header-message)
  "Insert 'header-message' in the beginning of 'buf-name' and display it."
  (save-current-buffer
    (save-excursion
      (set-buffer buf-name)
      (goto-char (point-min))
      (insert (concat header-message
                      "

=*=*=*=*=*=*=*=*=*=*=*=*

"))))
  (display-buffer buf-name))
;; (wysiwyg-tex-display-buffer-with-header "*scratch*" "hello from wysiwyg-tex")

(defun wysiwyg-tex-display-typesetting-err-log ()
  "Display error log while typesetting"
  (wysiwyg-tex-display-buffer-with-header
   (wysiwyg-tex-typesetting-log-buffer-name)
   (concat "["
           wysiwyg-tex-tex2dvi-command
           "] Failed in typesetting. See the typesetting log below.")))

(defun wysiwyg-tex-display-dvi2ps-err-log ()
  "Display error log while converting DVI into PS"
  (wysiwyg-tex-display-buffer-with-header
   (wysiwyg-tex-dvi2ps-log-buffer-name)
   (concat "["
           wysiwyg-tex-dvi2ps-command
           "] Failed in converting DVI into PS. See the log below.")))

(defun wysiwyg-tex-display-extract-ps-page-err-log ()
  "Display error log while extracting specific pages from PS"
  (wysiwyg-tex-display-buffer-with-header
   (wysiwyg-tex-extract-ps-page-log-buffer-name)
   (concat "["
           wysiwyg-tex-extract-ps-page-command
           "] Failed in extracting pages. See typesetting log below.")))



;;; Main
(defun wysiwyg-tex-insert-marker (texpath point)
  "Inserts (wysiwyg-tex-marker) on the 'point' of 'texpath' file.

@returns:
'texpath'"
  (save-current-buffer
    (save-excursion
      (let ((tex-buffer (find-file-noselect texpath)))
        (set-buffer tex-buffer)

        (goto-char point)
        (insert (wysiwyg-tex-marker-to-insert-in-tex))

        ;;         (goto-char (point-min))
        ;;         (insert "\\font\\minhatiwysiwygtex=min10 at 0.0001pt
        ;; \\font\\cmrhatiwysiwygtex=cmr10 at 0.0001pt
        ;; \\def\\WYSIWYG-TEX-MARKER-SIZE{\\minhatiwysiwygtex\\cmrhatiwysiwygtex}

        ;; ")
        (save-buffer)
        (kill-buffer tex-buffer))
      texpath)))

(defun wysiwyg-tex-copy-tex (src-texpath dest-texpath)
  "Copy file.

@returns:
'dest-texpath'"
  (call-process "cp" nil nil t src-texpath dest-texpath))

(defun wysiwyg-tex-copy-tex-with-marker (src-texpath dest-texpath pos-to-insert)
  "Copy TEX file and insert (wysiwyg-tex-marker) on 'pos-to-insert' in 'dest-texpath' file.

@returns:
'dest-texpath'"
  (call-process "cp" nil nil t src-texpath dest-texpath)
  (wysiwyg-tex-insert-marker dest-texpath pos-to-insert))

(defun wysiwyg-tex-find-page-with-marker (pspath)
  "Searches page with (wysiwyg-tex-marker) from 'pspath' file.

@returns:
Page number with (wysiwyg-tex-marker) by string"
  (save-current-buffer
    (save-excursion
      (let ((ps-buffer (find-file-noselect pspath)))
        (set-buffer ps-buffer)
        (goto-char (point-min))
        (search-forward ".,.,.")
        (search-backward-regexp "%%Page: [0-9]+ \\([0-9]+\\)")

        (let ((ret (match-string 1)))
          (kill-buffer ps-buffer)
          ret)))))
;; (wysiwyg-tex-find-page-with-marker "wysiwyg-tex-preview.ps")


(defun wysiwyg-tex-tex2ps (texpath)
  "Create PS file from 'texpath'.

@side-effect:
Open an error log buffer when one occurs.

@returns:
* Created PS file path on success.
* nil on failure."
  (message "Creating preview ...")

  (wysiwyg-tex-erase-buffer (wysiwyg-tex-typesetting-log-buffer-name))
  (if (not (eq (call-process wysiwyg-tex-tex2dvi-command nil
                             (wysiwyg-tex-typesetting-log-buffer-name) t
                             texpath)
               0))
      (progn (wysiwyg-tex-display-typesetting-err-log)
             nil)

    ;; Succeeded in 1st typesetting
    (wysiwyg-tex-erase-buffer (wysiwyg-tex-typesetting-log-buffer-name))

    (if wysiwyg-tex-typeset-3-times
        (progn
          (if (not (eq (call-process wysiwyg-tex-tex2dvi-command nil
                                     (wysiwyg-tex-typesetting-log-buffer-name) t
                                     texpath)
                       0))
              (progn (wysiwyg-tex-display-typesetting-err-log)
                     nil)

            ;; Succeeded in 2nd typesetting
            (wysiwyg-tex-erase-buffer (wysiwyg-tex-typesetting-log-buffer-name))
            (if (not (eq (call-process wysiwyg-tex-tex2dvi-command nil
                                       (wysiwyg-tex-typesetting-log-buffer-name) t
                                       texpath)
                         0))
                (progn (wysiwyg-tex-display-typesetting-err-log)
                       nil)

              ;; Succeeded in 3rd typesetting
              (if (not (eq (call-process wysiwyg-tex-dvi2ps-command nil
                                         (wysiwyg-tex-dvi2ps-log-buffer-name) t
                                         (wysiwyg-tex-tmp-dvi-name))
                           0))
                  (progn (wysiwyg-tex-display-dvi2ps-err-log)
                         nil)
                (wysiwyg-tex-whole-ps-name)))))

      ;; Compile just 1 time.
      (if (not (eq (call-process wysiwyg-tex-dvi2ps-command nil
                                 (wysiwyg-tex-dvi2ps-log-buffer-name) t
                                 (wysiwyg-tex-tmp-dvi-name))
                   0))
          (progn (wysiwyg-tex-display-dvi2ps-err-log)
                 nil)
        (wysiwyg-tex-whole-ps-name)))))
;; (wysiwyg-tex-tex2ps "hoge.tex")

(defun wysiwyg-tex-extract-page-with-marker (psname)
  "Extract a page with (wysiwyg-tex-marker) and create new file with it.

@side-effect:
Create a file named \"(wysiwyg-tex-marker)-extracted-preview.ps\" on pwd.

@parameters:
psname: PS file in which (wysiwyg-tex-marker) exists

@returns:
(wysiwyg-tex-extracted-ps-name)"
  (let ((page-with-marker (wysiwyg-tex-find-page-with-marker psname)))
    (if (not (eq (call-process wysiwyg-tex-extract-ps-page-command nil
                               (wysiwyg-tex-extract-ps-page-log-buffer-name) t
                               page-with-marker
                               psname (wysiwyg-tex-extracted-ps-name))
                 0))
        (wysiwyg-tex-display-extract-ps-page-err-log)
      (wysiwyg-tex-extracted-ps-name))))
;; (wysiwyg-tex-extract-page-with-marker "wysiwyg-tex-preview.ps")

(defun wysiwyg-tex-get-main-tex-src ()
  "Returns the full path of main TEX source."
  (defun wysiwyg-tex-main-src-name ()
    (shell-command-to-string (concat "cat "
                                     (wysiwyg-tex-main-src-reminder-name))))

  (cond ((or (not (file-exists-p (wysiwyg-tex-main-src-reminder-name)))
             (not (file-exists-p (wysiwyg-tex-main-src-name))))
         (let ((main-src-name (expand-file-name
                               (read-file-name "Enter your main source file: " nil nil t))))
           (shell-command (concat  "echo -n '" main-src-name "'"
                                   " > " (wysiwyg-tex-main-src-reminder-name)))
           main-src-name))

        (t (expand-file-name (wysiwyg-tex-main-src-name)))))
;; (wysiwyg-tex-get-main-tex-src)

(defun wysiwyg-tex-show-extracted-preview-after-typesetting (texpath)
  (let ((tmp-psname (wysiwyg-tex-tex2ps texpath)))
    (if (eq tmp-psname nil) (message "Failed in creating preview.")
      ;; When tex2ps succeeds
      (let* ((preview-psname (wysiwyg-tex-extract-page-with-marker tmp-psname))
             (preview-buffer (find-file-read-only-other-window preview-psname)))
        (set-buffer preview-buffer)
        (doc-view-toggle-display)))))

(defun wysiwyg-tex-show-preview ()
  "Create a preview of a page around which current cursor position is.

@side-effect:
* Display a preview with 1 page on success.
* Display an error log on failiur."
  (interactive)

  (defun wysiwyg-tex-show-preview-of-main-src (main-src-name)
    (wysiwyg-tex-copy-tex-with-marker main-src-name
                                      (wysiwyg-tex-tmp-tex-name)
                                      (point))
    (wysiwyg-tex-show-extracted-preview-after-typesetting (wysiwyg-tex-tmp-tex-name)))

  (defun wysiwyg-tex-show-preview-of-sub-src (sub-src-path main-src-path)
    (defun wysiwyg-tex-replace-all-tex-filename-in (path-or-name from-name to-name)
      (wysiwyg-tex-replace-all-in path-or-name from-name to-name)
      (wysiwyg-tex-replace-all-in path-or-name
                                  (progn (string-match "\\(.+\\)\.\\(tex\\|TEX\\)" from-name)
                                         (match-string 1 from-name))
                                  to-name))

    (wysiwyg-tex-copy-tex-with-marker sub-src-path
                                      (wysiwyg-tex-tmp-sub-tex-name)
                                      (point))
    (wysiwyg-tex-copy-tex main-src-path
                          (wysiwyg-tex-tmp-tex-name))
    (wysiwyg-tex-replace-all-tex-filename-in (wysiwyg-tex-tmp-tex-name)
                                             (wysiwyg-tex-extract-file-name-from-path sub-src-path)
                                             (wysiwyg-tex-tmp-sub-tex-name))
    (wysiwyg-tex-show-extracted-preview-after-typesetting (wysiwyg-tex-tmp-tex-name)))


  (wysiwyg-tex-kill-buffer-if-exists (wysiwyg-tex-whole-ps-name))
  (wysiwyg-tex-kill-buffer-if-exists (wysiwyg-tex-extracted-ps-name))

  (let ((main-src-name (wysiwyg-tex-get-main-tex-src)))
    (message "Creating preview...")
    (if (string-equal main-src-name (buffer-file-name))
        (wysiwyg-tex-show-preview-of-main-src main-src-name)
      (wysiwyg-tex-show-preview-of-sub-src (buffer-file-name) main-src-name))))


(defun wysiwyg-tex-show-whole-preview ()
  "Create a preview of the whole page..

@side-effect:
* Display a preview with whole page on success.
* Display an error log on failiur."
  (interactive)
  (wysiwyg-tex-kill-buffer-if-exists (concat (wysiwyg-tex-file-prefix) ".ps"))
  (let ((orig-texpath (wysiwyg-tex-get-main-tex-src)))
    (wysiwyg-tex-copy-tex orig-texpath (wysiwyg-tex-tmp-tex-name))

    (let ((psname (wysiwyg-tex-tex2ps (wysiwyg-tex-tmp-tex-name))))
      (if (eq psname nil) (message "Failed in creating preview.")

        ;; When tex2ps succeeds
        (let ((preview-buffer (find-file-read-only-other-window psname)))
          (set-buffer preview-buffer)
          (doc-view-toggle-display))))))

(provide 'wysiwyg-tex)
