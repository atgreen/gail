# gail
> An AI-powered GitHub Issue labeler

What is it?
------------
``gail`` will review the Issues in your GitHub-hosted project and,
where appropriate, assign new labels to the Issues from a list of
label options you provide. ``gail`` is currently configured to use
OpenAI LLMs to accomplish this.

You can also use `gail` in GitHub Actions to label all new incoming
Issues.  Checkout the easy-to-use GitHub Action here:
https://github.com/atgreen/gail-issue-labeler-action

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
NAME:
  gail - GitHub Automated Issue Labeler

USAGE:
  gail OWNER REPO | --action OWNER REPO

OPTIONS:
      --help           display usage information and exit
      --version        display version and exit
  -a, --action         Process a single issue from stdin instead of fetching from repository
  -l, --labels <PATH>  Labels file [default: .gail-labels]
  -m, --model <VALUE>  OpenAI model to use [default: gpt-4o-mini]
  -n, --dry-run        Show what would be labeled without actually labeling

EXAMPLES:

  Label issues in the libffi/libffi repository:

    gail libffi libffi

  Dry run with custom labels file:

    gail --labels my-labels.txt --dry-run microsoft vscode

  Use a different OpenAI model:

    gail --model gpt-4 owner repo

  Process a single issue from stdin:

    gail --action owner repo

  Process stdin issue with custom labels:

    gail --action --labels my-labels.txt owner repo

AUTHORS:
  Anthony Green <green@moxielogic.com>

LICENSE:
  MIT License
```

Note that you can simply commit your custom `.gail-labels` file to
your repo and run `gail` in your project's root directory in order to
pick up your custom labels.  As an example, here's the file I created
for libffi: https://github.com/libffi/libffi/blob/master/.gail-labels

Author and License
-------------------

``gail`` was written by [Anthony Green](https://github.com/atgreen),
and is distributed under the terms of the MIT license.
