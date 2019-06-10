(require 'xml)

;;; Code:

(setq elfeed-feeds
      '(
        ("https://forum.manjaro.org/c/announcements/stable-updates.rss" manjaro)
        "http://blog.rust-lang.org/feed.xml"
        ("http://news.ycombinator.com/rss" programming)
        ("http://www.techmeme.com/feed.xml" programming)
        ("http://this-week-in-rust.org/atom.xml" rust programming)
        ("http://www.canonical.com/rss.xml" programming)
        ("http://readrust.net/all/feed.rss" rust programming)
        ("http://feeds.feedburner.com/GiantRobotsSmashingIntoOtherGiantRobots" ruby)
        ("http://techblog.netflix.com/feeds/posts/default" netflix)
        ("http://weblog.rubyonrails.org/feed/atom.xml" ruby)
        ("http://feeds.feedburner.com/Rubyflow" ruby)
        ("http://feed.rutracker.org/atom/f/995.atom" rutracker philosophy)))

(use-package elfeed
    :ensure t
    :bind ("C-x w" . elfeed))

(use-package elfeed-web
    :ensure t)
;; (elfeed-load-opml "~/.emacs.d/feedly.opml")

(provide 'elfeed-module)
;;; elfeed-module.el ends here
