(require 'ox)
(require 'ox-publish)

;; Org Publish
(setq org-publish-use-timestamps-flag nil)
(setq org-export-time-stamp-file nil)
(setq org-export-with-broken-links t)
(setq org-export-with-date t)
(setq org-html-validation-link nil)
(setq org-export-html-validation-link nil)


(defun tikzjax-convert (backend)
  "Convert a latex org src block with tikz headers into a tizkjax html export block."
  (setq is-tikz nil)
  ;; Loop over src blocks and stops when there is no left
  (while (condition-case nil (org-babel-next-src-block)
           (error
            nil))

    ;; Extract relevant info from the org src block
    (setq src-info (org-babel-get-src-block-info))
    (let ((type (nth 0 src-info))
          (code (nth 1 src-info))
          (headers (seq-filter
                    (lambda (str)
                      (not (string= str ":headers")))
                    (assoc :headers (nth 2 src-info)))))

      ;; Check if it is a latex block with tikz on the headers
      (if (and (string= type "latex")
               (seq-contains-p headers "\\usepackage{tikz}" ))
          ;; Clean it up, change it to a html export and wrap it with the script tags
          (progn
            (setq is-tikz t)
            (org-babel-remove-result)
            (beginning-of-line)
            (kill-line)
            (insert "#+begin_export html")
            (end-of-line)
            (newline-and-indent)
            (insert "<script type=\"text/tikz\">")
            (search-forward-regexp "^\s*#\\+end_src\s*$")
            (beginning-of-line)
            (kill-line)
            (insert "#+end_export")
            (forward-line -1)
            (end-of-line)
            (newline-and-indent)
            (insert "</script>")))))
  ;; In the end, if it is a tikz src block add the proper html headers for tikzjax on this org file
  (if is-tikz (progn
                ;; Here im just trying to add it after all #+ on the beginning of the file
                (goto-char (point-min))
                (end-of-line)
                (newline-and-indent)
                (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://tikzjax.com/v1/fonts.css\">")
                (end-of-line)
                (newline-and-indent)
                (insert "#+HTML_HEAD: <script src=\"https://tikzjax.com/v1/tikzjax.js\"></script>"))))


(add-hook 'org-export-before-parsing-hook #'tikzjax-convert)


(defun org-export-collect-headlines (info &optional n scope)
  (let ((limit (plist-get info
                          :headline-levels)))
    (setq n (if (wholenump n)
                (min n limit) limit))
    (org-element-map (plist-get info
                                :parse-tree) 'headline #'(lambda (headline)
         (unless (or (org-element-property :NOTOC headline) ; new condition
                     (org-element-property :footnote-section-p headline)) ; old condition
           (let ((level (org-export-get-relative-level headline info)))
             (and (<= level n)
                  headline))))
      info)))

(setq org-publish-project-alist '(("mattf.tk" :base-directory "./."
                                   :base-extension "org"
                                   :publishing-directory "./html"
                                   :with-creator t
                                   :with-timestamps nil
                                   :validate nil
                                   :auto-sitemap t
                                   :recursive t
                                   :exclude "template.org\\|README.org\\|sitemap.org"
                                   :publishing-function org-html-publish-to-html
                                   :headline-levels 4
                                   :html-postamble
                                   "
                                        <h2>﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏</h2>
                                        <p class=\"title\"><a href=\"#top\">%t</a></p>
                                        <p class=\"author\">Author: %a</p>
                                        <p class=\"date\">Creation Date: %d</p>
                                        <p>Modified: %C</p>
                                        <p>View this page on <a id=\"githubref\" href=\"https://github.com/matheusfillipe/myblog/
\">github</a></p>
                                        <p class=\"creator\">%c</p>"
                                   :auto-preamble t)))

(org-publish-all t)

(message "Build complete!")
