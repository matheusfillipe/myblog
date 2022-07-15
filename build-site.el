;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)

(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)
(package-install 's)
(require 's)
(require 'ox-publish)

;; Org Publish Config
(setq org-publish-use-timestamps-flag nil
      org-export-time-stamp-file nil
      org-export-with-broken-links t
      org-export-with-date t
      org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-doctype "html5"
      org-export-html-validation-link nil)

(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "org-")

(defun match-tikz-packages (headers)
  (let ((str (car headers)))
    (when (string-match "\\usetikzlibrary{\\(.*?\\)}" str)
      (when (match-string 1 str)
        (match-string 1 str)))))

(defun match-tex-packages (headers)
  (let ((str (car headers)))
    (when (string-match "\\usepackage{\\(.*?\\)}" str)
      (when (match-string 1 str)
        (match-string 1 str)))))

(defun tikzjax-convert ()
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
      (if (and headers (string= type "latex")
               (member "tikz" (split-string (match-tex-packages headers) ",")))
          ;; Clean it up, change it to a html export and wrap it with the script tags
          (progn
            (setq is-tikz t)
            (org-babel-remove-result)
            (beginning-of-line)
            (kill-line)
            (insert "#+begin_export html")
            (end-of-line)
            (newline-and-indent)
            (insert "<div class=\"tikzjax\">")
            ;; (print (format "---> packages=|%s|" (seq-filter
            ;;                                      (lambda (x)
            ;;                                        (not (string= x "tikz")))
            ;;                                      (split-string (match-tex-packages headers) ","))))
            ;; TODO add data-tex-package='{"pgfplots":"","custom-package":"option=special"}'>
            (if (match-tikz-packages headers)
                (insert (format "<script type=\"text/tikz\" data-tikz-libraries=\"%s\">" (match-tikz-packages headers)))
              (insert "<script type=\"text/tikz\">"))
            (search-forward-regexp "^\s*#\\+end_src\s*$")
            (beginning-of-line)
            (kill-line)
            (insert "#+end_export")
            (forward-line -1)
            (end-of-line)
            (newline-and-indent)
            (insert "</script></div>")))))
  ;; In the end, if it is a tikz src block add the proper html headers for tikzjax on this org file
  (if is-tikz (progn
                ;; Here im just trying to add it after all #+ on the beginning of the file
                (goto-char (point-min))
                (end-of-line)
                (newline-and-indent)
                ;; Add tikzjax headers to page
                (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://tikzjax.pages.dev/fonts.css\">")
                (end-of-line)
                (newline-and-indent)
                (insert "#+HTML_HEAD: <script src=\"https://tikzjax.pages.dev/tikzjax.js\"></script>"))))


(defun org-export-collect-headlines (info &optional n scope)
  "Collect headlines for export."
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

(defun my/org-html-format-headline-function (todo todo-type priority text tags info)
  "Format a headline with a link to itself."
  (let* ((headline (get-text-property 0 :parent text))
         (id (or (org-element-property :CUSTOM_ID headline)
                 (org-export-get-reference headline info)
                 (org-element-property :ID headline)))
         (link (if id
                   (format "<a href=\"#%s\">%s</a>" id text)
                 text)))
    (org-html-format-headline-default-function todo todo-type priority link tags info)))

(defun my/title-to-filename (title)
  "Convert TITLE to a reasonable filename."
  ;; Based on the slug logic in org-roam, but org-roam also uses a
  ;; timestamp, and I use only the slug. BTW "slug" comes from
  ;; <https://en.wikipedia.org/wiki/Clean_URL#Slug>
  (setq title (s-downcase title))
  (setq title (s-replace-regexp "[^a-zA-Z0-9]+" "-" title))
  (setq title (s-replace-regexp "-+" "-" title))
  (setq title (s-replace-regexp "^-" "" title))
  (setq title (s-replace-regexp "-$" "" title))
  title)

(defun slug-name (slug existing-ids)
  "Return a unique slug name for SLUG."
  (let* ((duplicate-id (member slug existing-ids)))
    (if duplicate-id (slug-name (format "%s-" slug) existing-ids) slug)))

(defun my/org-generate-custom-ids ()
  "Generate CUSTOM_ID for any headings that are missing one"
  (let ((existing-ids (org-map-entries
                     (lambda () (org-entry-get nil "CUSTOM_ID")))))
    (org-map-entries
     (lambda ()
       (let* ((custom-id (org-entry-get nil "CUSTOM_ID"))
              (heading (org-heading-components))
              (level (nth 0 heading))
              (todo (nth 2 heading))
              (headline (nth 4 heading))
              (slug (my/title-to-filename headline)))
         (when (and (not custom-id)
                    (< level 5)
                    (not todo))
           (let* ((new-id (slug-name slug existing-ids)))
             (push new-id existing-ids)
             (org-entry-put nil "CUSTOM_ID" new-id))))))))


(defun preprocess (backend)
  "Preprocess the org file before exporting."
  (tikzjax-convert)
  (my/org-generate-custom-ids))

(add-hook 'org-export-before-parsing-hook #'preprocess)


(setq org-publish-project-alist '(("mattf.one" :base-directory "./."
                                   :base-extension "org"
                                   :publishing-directory "./html"
                                   :with-creator t
                                   :with-timestamps nil
                                   :validate nil
                                   :auto-sitemap t
                                   :recursive t
                                   :exclude "template.org\\|README.org\\|sitemap.org"
                                   :publishing-function org-html-publish-to-html
                                   :html-format-headline-function my/org-html-format-headline-function
                                   :headline-levels 4
                                   :htmlized-source t
                                   :html-postamble "
                                        <h2>﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏﹏</h2>
                                        <p class=\"title\"><a href=\"#top\">%t</a></p>
                                        <p class=\"author\">Author: %a</p>
                                        <div class=\"postamble-left\">
                                        <p class=\"date\">Creation Date: %d</p>
                                        <p>Modified: %C</p>
                                        </div>
                                        <div class=\"postamble-right\">
                                        <p>View this page on <a id=\"githubref\" href=\"https://github.com/matheusfillipe/myblog/
\">github</a></p>
                                        <p class=\"creator\">%c</p>
                                        </div>



                                        <ul class=\"rightNav\">
                                        <span class=\"topbar-menu\"><a class=\"nobox\" href=\"https://www.instagram.com/pawns4love/\"><img title=\"Check out my pets on instagram!\" src=\"/assets/insta.png\"></a></span>
                                        <span class=\"topbar-menu\"><a class=\"nobox\" href=\"https://open.spotify.com/user/flylfylfight\"><img title=\"Check out what I like to listen to\" src=\"/assets/spotify.png\"></a></span>
                                        <span class=\"topbar-menu\"><a class=\"nobox\" href=\"https://www.youtube.com/channel/UC9v3ZuKniNaDvLrhffnTt9A\"><img title=\"My automated videos channel\" src=\"/assets/youtube.png\"></a></span>
                                        <span class=\"topbar-menu\"><a class=\"nobox\" href=\"https://github.com/matheusfillipe/myblog\"><img title=\"View this blog repo's in github\" src=\"/assets/github.png\"></a></span>
                                        </ul>"

                                   :auto-preamble t)))

(org-publish-all t)

(message "Build complete!")
