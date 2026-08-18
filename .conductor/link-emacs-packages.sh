#!/usr/bin/env bash
# Reuse the canonical ~/.emacs.d packages in a Conductor workspace instead of
# re-downloading them: symlink the heavy, regenerable dirs to the repo root.
#
# Runs automatically via .conductor/settings.toml [scripts] setup, and can be
# run by hand from any workspace:  bash .conductor/link-emacs-packages.sh
set -euo pipefail

# Canonical config. Conductor sets these; fall back for manual runs.
SRC="${CONDUCTOR_ROOT_PATH:-$HOME/.emacs.d}"
DEST="${CONDUCTOR_WORKSPACE_PATH:-$(cd "$(dirname "$0")/.." && pwd)}"

# Never touch the canonical dir itself (would delete the real packages).
if [ "$(cd "$SRC" && pwd)" = "$(cd "$DEST" && pwd)" ]; then
  echo "link-emacs-packages: workspace is the canonical config; nothing to do."
  exit 0
fi

# ponytail: shares straight/ and eln-cache/ wholesale — kills the download AND
# recompile. Ceiling: two Emacs rebuilding packages at once race on build/.
# If that bites, link only straight/repos and keep build/ + eln-cache/ local.
for dir in straight eln-cache; do
  if [ ! -d "$SRC/$dir" ]; then
    echo "link-emacs-packages: $SRC/$dir missing, skipping" >&2
    continue
  fi
  rm -rf "${DEST:?}/$dir"
  ln -s "$SRC/$dir" "$DEST/$dir"
  echo "linked $dir -> $SRC/$dir"
done
