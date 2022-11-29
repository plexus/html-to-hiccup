(require 'ert)
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
    (buffer-substring start (min end (point-max)))))


(defun html-to-hiccup-test-case (html hiccup)
  (should (string= hiccup
                   (set-up-buffer html
                                  "C-SPC M-> M-x html-to-hiccup-convert-region"
                                  1
                                  (1+ (length hiccup))))))

(ert-deftest html-to-hiccup-convert-region-basic-test ()
  "HTML to Hiccup conversion test"
  (html-to-hiccup-test-case
   "<html>
      <body>
        <h1>Yes</h1>
        <p>foo</p>
      </body>
    </html>"
   "[:html [:body [:h1 \"Yes\"] [:p \"foo\"]]]"))

(ert-deftest html-to-hiccup-convert-region-class-id-test ()
  "HTML to Hiccup conversion test"
  (html-to-hiccup-test-case
   "<html>
      <body id=\"foo\">
        <h1 class=\"header big\">Yes</h1>
        <p>foo</p>
      </body>
    </html>"
   "[:html [:body#foo [:h1.header.big \"Yes\"] [:p \"foo\"]]]"))

(ert-deftest html-to-hiccup-class-special-chars-test ()
  "HTML to Hiccup conversion test"
  (html-to-hiccup-test-case
   "<html>
      <body id=\"foo\">
        <h1 class=\"max-w-sm w-full sm:w-1/2 lg:w-1/3 py-6 px-3\">Yes</h1>
        <p>foo</p>
      </body>
    </html>"
   "[:html [:body#foo [:h1 {:class \"max-w-sm w-full sm:w-1/2 lg:w-1/3 py-6 px-3\"} \"Yes\"] [:p \"foo\"]]]"))

(ert-deftest html-to-hiccup-convert-region-attrs-test ()
  "HTML to Hiccup conversion test"
  (html-to-hiccup-test-case
   "<html>
      <body>
        <h1 class=\"header big\" aria-label=\"Yes\">Yes</h1>
        <p style=\"border: 1px solid black;\" foo=bar>foo</p>
      </body>
    </html>"
   "[:html [:body [:h1.header.big {:aria-label \"Yes\"} \"Yes\"] [:p {:style \"border: 1px solid black;\" :foo \"bar\"} \"foo\"]]]"))

(ert-deftest html-to-hiccup-convert-no-shorthand-test ()
  "HTML to Hiccup conversion test"
  (let ((html-to-hiccup-use-shorthand-p nil))
      (html-to-hiccup-test-case
       "<html>
         <body>
           <h1 class=\"header big\" aria-label=\"Yes\">Yes</h1>
           <p style=\"border: 1px solid black;\" foo=bar>foo</p>
         </body>
       </html>"
       "[:html [:body [:h1 {:class \"header big\" :aria-label \"Yes\"} \"Yes\"] [:p {:style \"border: 1px solid black;\" :foo \"bar\"} \"foo\"]]]")))
