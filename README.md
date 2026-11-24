# reddit-reader

A read-only Reddit reader for Emacs, built for low vision.

I have Stargardt disease and read at an 80-point font, about 39 characters per line. Reddit's own pages don't work at that size, so this shows Reddit the way my mail reader shows mail: a plain list of communities, a plain list of posts, then one post and its comments in very large type, with read-aloud.

## What it does

- Lists the subreddits named in `~/.config/reddit-reader/subreddits.txt`.
- Fetches one subreddit listing when I open a community, and one comment thread when I open a post. Every request follows a keypress; nothing runs in the background.
- Caches what it fetched in SQLite on my own machine (`~/.local/share/reddit-reader/`).
- Keeps stars and read-marks locally. They are never sent to Reddit.
- Reads a post and its top comments aloud.

## What it does not do

It has no code to vote, post, comment, message, follow or moderate. It does not collect, share, republish or analyze Reddit data, and nothing is used to train a model. One user, non-commercial.

## Status

The reader and its tests are written (`make test`). It cannot load anything today because Reddit no longer serves unauthenticated requests. I have asked Reddit for API access as an accessibility tool; if approved, the network layer moves to app-only OAuth with the same two read requests.

The test fixtures are made-up posts, not Reddit content.

## Use

    make test
    make compile

Then load `emacs/reddit-reader.el` and run `M-x reddit-reader`.
