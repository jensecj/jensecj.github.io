(require 'org)
(require 'ox-html)
(require 'ox-publish)
(require 'ox-rss)

(require 's)
(require 'f)
(require 'dash)

(defun blog--reload-firefox ()
  "Reaload visible firefox browser using `xdotool'"
  (interactive)
  ;; TODO: should be more specific, maybe using window id?
  (let ((current-window (s-trim (shell-command-to-string "xdotool getactivewindow"))))
    (shell-command "xdotool search --onlyvisible --classname Navigator windowactivate --sync key F5")
    (shell-command (format "xdotool windowactivate %s" current-window))))

(defun blog--head ()
  "Contents of the <head> tag."
  (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"/res/css/style.css\" />\n"
          "<link rel=\"icon\" type=\"image/png\" href=\"/res/logo.png\">"))

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
  "Footer content"
  "<footer> </footer>")

(defun blog-sitemap-format-entry (entry _style project)
  "Return string for each ENTRY in PROJECT."
  (when (s-starts-with-p "posts/" entry)
    (let* ((file (org-publish--expand-file-name entry project))
           (filename (f-no-ext (f-filename file)))
           (date (substring filename 0 10))
           (title (s-replace "-" " " (substring filename 11))))
      (format "@@html:<span class=\"archive-item\"><span class=\"archive-date\">@@ %s @@html:</span>@@ [[file:%s][%s]] @@html:</span>@@"
              date entry title))))

(defun blog-sitemap-function (title list)
  "Return sitemap using TITLE and LIST returned by `org-blog-sitemap-format-entry'."
  (concat
   "\n#+begin_archive\n"
   (mapconcat (lambda (li)
                (format "@@html:<li>@@ %s @@html:</li>@@" (car li)))
              (seq-filter #'car (cdr list))
              "\n")
   "\n#+end_archive\n"))


(defun blog-publish (force)
  "Publish `blog' target using `org-publish'.
Force publish all files if called with `prefix-argument'."
  (interactive "P")
  (save-excursion
    (let ((org-html-htmlize-output-type 'css)
          (org-confirm-babel-evaluate nil)
          (org-export-babel-evaluate t)
          (org-export-use-babel t)

          (org-publish-project-alist
           `(("blog-org-to-html"
              :recursive t
              :base-directory "~/vault/blog/src/blog/"
              :base-extension "org"
              :publishing-directory "~/vault/blog/"
              :publishing-function org-html-publish-to-html

              :html-head ,(blog--head)
              :html-home/up-format ,(blog--header)
              :html-preamble nil
              :html-postamble blog--footer
              :html-head-include-scripts nil
              :html-html5-fancy t
              :html-doctype "html5"
              :html-link-home "/"
              :html-link-up "/"

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
              :sitemap-format-entry blog-sitemap-format-entry
              :sitemap-function blog-sitemap-function)
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
             ("blog" :components ("blog-org-to-html" "blog-assets"))
             )))
      (org-publish-project "blog" force)
      (blog-reload-firefox))))

(provide 'blog)
