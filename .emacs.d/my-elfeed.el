(require-package 'elfeed)
(require 'elfeed)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '(

        ;; Planets
        "http://planet.nixos.org/atom.xml"
        "http://planet.haskell.org/atom.xml"
        
        ;; Math
        "http://scientopia.org/blogs/goodmath/feed/"
        "http://theorylunch.wordpress.com/feed/"
        "http://existentialtype.wordpress.com/feed/"

        ;; Misc
        "http://noamlewis.wordpress.com/feed/"
        "http://www.yesodweb.com/feed"

        ;; Packaging
        "https://github.com/fernandotcl/udisks-glue/releases.atom"
        "https://github.com/Gottox/bgs/releases.atom"
        "https://bitbucket.org/portix/dwb/rss"
       ))

(dolist (regex '("github.*udisks-glue"
                 "github.*bgs"
                 "bitbucket.*dwb"))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url regex
                                :add '(packaging))))

(provide ' my-elfeed)
