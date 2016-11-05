# rssparser.lisp

A Web-to-RSS parser in Common Lisp.

## Syntax

* *chmod +x rssparser.lisp*, then:
  * ./rssparser.lisp add *<Title> <URL> <EntrySelector> <TitleSelector> [<ContentSelector>]*
  * ./rssparser.lisp delete *<ID>*
  * ./rssparser.lisp list

***Run a simple web interface on port 5000:***

* ./rssparser.lisp webserver

(*Note:* Currently the built-in web server can only list and delete feeds. Adding new feeds will come later.)

***Cronjob or manual feed creation command:***

* ./rssparser.lisp parse

Supported *selectors* are all valid CSS selectors. If you don't specify a `ContentSelector` when adding a new feed, `rssparser.lisp` will use "Generated with rssparser.lisp." as every feed item's body.

### Example

If you want to subscribe to the [KiTTY website](http://www.9bis.net/kitty/?action=news&zone=en):

    % ./rssparser.lisp add "KiTTY" "http://www.9bis.net/kitty/?action=news&zone=en" ".news" "h1" ""
    Success!
    
    % ./rssparser.lisp parse
    
    % ./rssparser.lisp list
    1 feed is set up:
    
    ID: 23  Title:        KiTTY
            URL:          http://www.9bis.net/kitty/?action=news&zone=en
            Last success: Sun, 27 Mar 2016 17:54:18 +0200
            
By default, the KiTTY website feed will be stored as `feeds/feed23.xml` then.

## Requirements

[SBCL](http://www.sbcl.org) with [Quicklisp](http://www.quicklisp.org) set up. Also, [SQLite3](http://www.sqlite3.org) with a database file ready. Also, you should create a folder where your feed files should be created.

### SQLite schema

The feeds.db file has the following schema:

    CREATE TABLE feeds (
      id integer primary key autoincrement,
      feedtitle text not null,
      url text not null,
      entryselector text not null,
      titleselector text not null,
      contentselector text not null,
      lastsuccess integer
    );
    
    CREATE TABLE entries (
      id integer primary key autoincrement,
      feedid integer,
      title text not null,
      contents blob,
      url text not null,
      timestamp integer
    );


## Configuration

You can set a couple of parameters in the `config.lisp` file:

* `+database-file+`: The SQLite database file. (Default: `feeds.db`.) Note that this file *needs* to be accessible for the RSS parser to work!
* `+feed-folder+`: The folder where the feed files should be created. (Default: `feeds/`.) The script *needs* to be able to create files there; it checks its permissions automatically and informs you if it needs some help.
* `+max-items-per-feed+`: The maximum number of items per feed. (Default: `50`.)
* `+feed-cleanup+`: If set to `t` (which is the default value), the `entries` table will automatically be purged from old entries (only *2 * `+max-items-per-feed+`* are kept). Set this to `nil` if you want to bloat your database.
* `+remove-dead-feeds+`: If set to `t`, a website which is not reachable anymore will automatically be removed from your feed list. The parser will inform you of that so if you run `rssparser.lisp` as a cronjob, you'll see what happened in your logfiles.
* `+webserver-port+`: The port to run the webserver on when `rssparser.lisp webserver` is executed. It should be available through your firewall. (Default: `5000`.)
