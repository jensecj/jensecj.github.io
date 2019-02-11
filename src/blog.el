(require 'org)
(require 'ox-html)
(require 'ox-publish)
(require 'ox-rss)

(require 's)
(require 'f)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun fmt (strings &rest args)
  "Accept format string as a list of strings, which will be
joined with a newline."
  (apply #'format (s-join "\n" strings) args))

(defun blog--reload-firefox ()
  "Reaload visible firefox browser using `xdotool'"
  ;; TODO: should be more specific, maybe using window id?
  (let ((current-window (s-trim (shell-command-to-string "xdotool getactivewindow"))))
    (shell-command "xdotool search --onlyvisible --classname Navigator windowactivate --sync key F5")
    (shell-command (format "xdotool windowactivate %s" current-window))))

(defun blog--find-org-files (path)
  "Resursively find all .org files at PATH."
  (f-entries path #'(lambda (f) (f-ext? f "org")) t))

(defun blog-find-posts-file ()
  "Visit a posts file."
  (interactive)
  (let* ((drafts (blog--find-org-files "~/vault/blog/src/drafts/"))
         (posts (blog--find-org-files "~/vault/blog/src/blog/posts/"))
         (pick (completing-read "Find post: " (-concat drafts posts))))
    (when (and pick (f-exists? pick))
      (find-file pick))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; construction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun blog--sitemap-format-entry (entry _style project)
  "Return string for each ENTRY in PROJECT."
  (when (s-starts-with? "posts/" entry)
    (let* ((file (org-publish--expand-file-name entry project))
           (filename (f-no-ext (f-filename file)))
           (date (substring filename 0 10))
           (title (s-replace "-" " " (substring filename 11))))
      (fmt
       '("@@html:<span class=\"archive-item\"><span class=\"archive-date\">@@"
         "%s @@html:</span>@@"
         "[[file:%s][%s]]"
         "@@html:</span>@@")
       date entry title))))

(defun blog--sitemap-function (title plist)
  "Return sitemap using TITLE and PLIST returned by `org-blog-sitemap-format-entry'."
  (concat
   "\n#+begin_archive\n"
   (mapconcat (lambda (li)
                (format "@@html:<li>@@ %s @@html:</li>@@" (car li)))
              (seq-filter #'car (cdr plist))
              "\n")
   "\n#+end_archive\n"))

(defun blog--link-resource (rel type href)
  "Create a html string for linking a resource."
  (format "<link rel=\"%s\" type=\"%s\" href=\"/res/%s\" />\n" rel type href))

(defun blog--head ()
  "Contents of the <head> tag."
  (concat (blog--link-resource "stylesheet" "text/css" "css/style.css")
          (blog--link-resource "icon" "image/png" "logo.png")))

(defun blog--header ()
  "Header content, including navigation."
  "<header>
    <nav>
     <ul>
      <li><a href=\"/\"> index </a></li>
      <li><a href=\"/about.html\"> about </a></li>
     </ul>
   </nav>
</header>")

(defun blog--footer (_plist)
  "Footer content."
  "<footer></footer>")

(defun blog-publish (force)
  "Publish `blog' target using `org-publish'.
Force publish all files if called with `prefix-argument'."
  (interactive "P")
  (save-excursion
    (let (;; export css-selectors for things, makes styling code-blocks simple.
          (org-html-htmlize-output-type 'css)

          ;; eval code blocks and dont ask about it
          (org-export-use-babel t)
          (org-confirm-babel-evaluate nil)

          (org-publish-project-alist
           `(("blog-org-files"
              :recursive t
              :base-directory "~/vault/blog/src/blog/"
              :base-extension "org"
              :publishing-directory "~/vault/blog/"
              :publishing-function org-html-publish-to-html

              :html-head ,(blog--head)
              :html-home/up-format ,(blog--header)
              :html-preamble nil
              :html-postamble blog--footer
              :html-html5-fancy t
              :html-doctype "html5"
              :html-link-home "/"
              :html-link-up "/"
              :html-head-include-default-style t
              :html-head-include-scripts nil

              :with-toc nil
              :with-author t
              :with-title t
              :with-date t
              :with-footnotes t
              :section-numbers nil

              :auto-sitemap t
              :sitemap-filename "archive.org"
              :sitemap-style list
              :sitemap-sort-files anti-chronologically
              :sitemap-format-entry blog--sitemap-format-entry
              :sitemap-function blog--sitemap-function)
             ("blog-rss"
              :base-directory "~/vault/blog/src/blog/"
              :base-extension "org"
              :html-link-home "https://jensecj.github.io/"
              :html-link-use-abs-url t
              :rss-extension "xml"
              :publishing-directory "~/vault/blog/"
              :publishing-function (org-rss-publish-to-rss)
              :section-numbers nil
              :exclude ".*"
              :include ("archive.org")
              :table-of-contents nil)
             ("blog-assets"
              :recursive t
              :base-directory "~/vault/blog/src/blog/"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
              :publishing-directory "~/vault/blog/"
              :publishing-function org-publish-attachment)
             ("blog" :components ("blog-org-files" "blog-rss" "blog-assets")))))
      (org-publish-project "blog" force)
      (message "Finished publishing"))))

(provide 'blog)
