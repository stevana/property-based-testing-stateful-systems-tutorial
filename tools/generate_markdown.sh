#!/usr/bin/env bash

set -euo pipefail
if [[ "${TRACE-0}" == "1" ]]; then set -o xtrace; fi

SCRATCH=${SCRATCH:-"/tmp/pbt-stateful-systems"}

# Change directory to the root directory of this repo. Taken from:
# https://stackoverflow.com/a/246128/3858681
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" > /dev/null 2>&1 && pwd )"
pushd "${DIR}/.." > /dev/null

PANDOC="pandoc --wrap=none --highlight-style=kate"

# Backup old docs before replacing them.
mv ./docs "${SCRATCH}-docs-backup-$(date +%s)"
mkdir -p docs

for file in src/*.lhs; do
    ${PANDOC} --from markdown+lhs --to gfm "${file}" > "docs/$(basename ${file} .lhs).md"
    sed -i 's/``` {.haskell .literate}/```haskell/'    "docs/$(basename ${file} .lhs).md"
done

if [[ "${1:-}" == "--preview-html" ]]; then

  mkdir -p "${SCRATCH}-html-preview/docs"

  ${PANDOC} --from gfm --to html README.md > "${SCRATCH}-html-preview/README.html"
  sed -i 's/.md/.html/g' "${SCRATCH}-html-preview/README.html"

  cp -r images/ "${SCRATCH}-html-preview/"

  for file in docs/*.md; do
      ${PANDOC} --from gfm --to html "${file}" > \
                "${SCRATCH}-html-preview/docs/$(basename ${file} .md).html"
      sed -i 's/.md/.html/g' "${SCRATCH}-html-preview/docs/$(basename ${file} .md).html"
  done

  echo "Preview unstyled HTML: ${SCRATCH}-html-preview/README.html"

fi

popd > /dev/null
