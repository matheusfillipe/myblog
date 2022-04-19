(require 'ox-publish)

;; Org Publish
(setq org-publish-use-timestamps-flag nil)
(setq org-export-time-stamp-file nil)
(setq org-export-with-broken-links t)
(setq org-export-with-date t)
(setq org-html-validation-link nil)
(setq org-export-html-validation-link nil)

(defun org-export-collect-headlines (info &optional n scope)
  (let ((limit (plist-get info
                          :headline-levels)))
    (setq n (if (wholenump n)
                (min n limit) limit))
    (org-element-map (plist-get info
                                :parse-tree) 'headline
      #'(lambda (headline)
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
                                   :html-postamble "
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
