(load-file "html-to-hiccup.el")

(defun set-up-buffer (str keys start end)
  "STR populate the buffer with
   KEYS are commands to type
   START & END are points from which the content of the buffer should be returned"
  (with-temp-buffer
    (insert str)
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (goto-char (point-min))
      (execute-kbd-macro (kbd keys)))
    (buffer-substring start end)))

(defconst html-content "<html>
                          <body>
                            <h1>Yes</h1>
                            <p>foo</p>
                          </body>
                        </html>")

(ert-deftest html-to-hiccup-convert-region-test ()
  "HTML to Hiccup conversion test"
  (should (string= "[:html [:body [:h1 \"Yes\"] [:p \"foo\"]]]"
                   (set-up-buffer html-content
				  "C-SPC M-> M-x html-to-hiccup-convert-region"
                                1
                                39))))
(ert t)
