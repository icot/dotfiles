
(use-package elfeed
  :defer t
  :config
  (setq-default elfeed-search-filter "+unread ")
  (setq elfeed-feeds
	'("http://martinfowler.com/feed.atom"
	  "http://www.schneier.com/blog/index.rdf"
	  "http://lwn.net/headlines/newrss"
	  "http://lambda-the-ultimate.org/rss.xml"
	  "http://feeds.feedburner.com/codinghorror/"
	  "http://blog.jgc.org/feeds/posts/default"
	  "https://googleprojectzero.blogspot.com/feeds/posts/default"
	  "http://planet.clojure.in/atom.xml"
	  "http://planet.lisp.org/rss20.xml"
	  "http://blog.fogus.me/feed/"
	  "https://insideclojure.org/feed.xml"
	  "https://jao.io/blog/rss.xml"
	  "https://www.exploit-db.com/rss.xml"
	  "https://www.tandfonline.com/feed/rss/ucry20"
	  "https://writings.stephenwolfram.com/feed/"
	  "https://drewdevault.com/blog/index.xml"
	  "https://oxide.computer/blog/feed"
	  "https://sachachua.com/blog/feed/")))
