# gail
> An AI-powered GitHub Issue labeler

What is it?
------------
``gail`` will review the Issues in your GitHub-hosted project and,
where appropriate, assign new labels to the Issues from a list of
label options you provide. ``gail`` is currently configured to use
OpenAI LLMs to accomplish this.

I wrote `gail` to add some semblance of order to [libffi's ~200 open
Issues](https://github.com/libffi/libffi/issues).  It exceeded
expectations, and I hope you enjoy it as well!

Building `gail`
----------------
Download all dependencies using [`ocicl`](https://github.com/ocicl/ocicl).
```
$ ocicl install
```
Now run `make`.  You are done.

Using `gail`
-------------

Labeling Issues has never been easier.  Here's the `gail` help message
with all of the instructions that you'll need:

```
gail 1.0 - copyright (C) 2025 Anthony Green <green@moxielogic.com>

Usage: ./gail [OPTIONS] OWNER REPO

Options:
  -l, --labels FILE     Labels file (default: labels.txt)
  -n, --dry-run         Show what would be labeled without actually labeling
  -m, --model MODEL     OpenAI model to use (default: gpt-4o-mini)
  -h, --help            Show this help message

Arguments:
  OWNER                 GitHub repository owner
  REPO                  GitHub repository name

Environment variables:
  GITHUB_TOKEN          GitHub API token (required)
  OPENAI_KEY            OpenAI API key (required)

Examples:
  ./gail libffi libffi
  ./gail --labels my-labels.txt --dry-run microsoft vscode

Distributed under the terms of the MIT License
```


Author and License
-------------------

``gail`` was written by [Anthony Green](https://github.com/atgreen),
and is distributed under the terms of the MIT license.
