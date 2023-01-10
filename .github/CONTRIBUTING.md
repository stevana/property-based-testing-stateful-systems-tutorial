# Contributing

## Before making a change

* Please open issue before embarking on a big change, in order to avoid
  unnecessary or duplicate work.

## Before making a pull request

* We roughly follow the conventional commit message
  [format](https://www.conventionalcommits.org/en/v1.0.0/), please have a look
  at `git log` and try to mimic along (we make use of the format when generating
  the changelog, see "making a new release" below);

* If the change involves changes to the markdown, then it could be useful to
  preview generated HTML with `tools/generate_markdown.sh --preview-html`. Note
  that the generated HTML isn't styled, so it won't be looking as nice as once
  uploaded to GitHub, but still useful for checking the formatting;

* Please check the GitHub actions CI configuration in
  [`.github/workflows/ci.yaml`](./workflows/ci.yaml) for which steps and checks
  are done as part of CI and try to make them part of your development workflow.
  The [`act`](https://github.com/nektos/act) tool could be useful for running
  all of CI locally as a final check before pushing your branch.

## Making a new release

To create a new release do the following steps:

```bash
    export NEW_VERSION=X.Y.Z # Try to follow semantic versioning.

    export LAST_VERSION="$(git tag | sort | tail -1)"
    ./tools/generate_changelog.hs "${LAST_VERSION}" | \
        sed "s/\[HEAD\]/\[v${NEW_VERSION}\]/" | \
        sed "s/\.\.\.HEAD/...v${NEW_VERSION}/" > /tmp/NEW_CHANGELOG.md
    cat CHANGELOG.md >> /tmp/NEW_CHANGELOG.md
    mv /tmp/NEW_CHANGELOG.md CHANGELOG.md

    git diff # Check that everything looks alright.
    git checkout -b update-changelog-"${NEW_VERSION}"
    git add CHANGELOG.md
    git commit -m "docs: update changelog for release v${NEW_VERSION}"
    gh pr create # Or `git push` and create a PR via the web UI.

    # Merge the PR.

    git checkout main
    git pull
    git branch -d update-changelog-"${NEW_VERSION}"

    git tag -a "v${NEW_VERSION}" -m "tag: v${NEW_VERSION}"
    git push origin "v${NEW_VERSION}"
```

Upon pushing the tag the
[`.github/workflows/release.yaml`](./workflows/release.yaml) workflow will kick
off and upload the build artifacts to the releases page.
