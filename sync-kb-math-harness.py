#!/usr/bin/env python3

"""Render the canonical KB math-harness note as an evergreen site page."""

from __future__ import annotations

import argparse
import os
import re
from pathlib import Path


def render(source: str) -> str:
    lines = source.splitlines()
    if not lines or lines[0] != "# The Math Harness":
        raise ValueError("expected the KB note to start with '# The Math Harness'")

    body = lines[1:]
    while body and not body[0]:
        body.pop(0)

    headings = [line for line in body if line.startswith("## ")]
    expected = ["## Benchmark", "## Harness", "## Problem Specific Harness"]
    if headings != expected:
        raise ValueError(f"expected exactly these main sections: {expected}")

    demoted = [
        re.sub(r"^(#{2,6})(?= )", lambda match: match.group(1)[1:], line)
        for line in body
    ]
    frontmatter = [
        "---",
        "title: The Math Harness",
        "---",
        "",
        "<!-- Generated from ~/kb/notes/essays/math-harness.md. Edit the KB note, then rerun sync-kb-math-harness.py. -->",
        "",
    ]
    return "\n".join(frontmatter + demoted) + "\n"


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--check",
        action="store_true",
        help="fail if the generated page is not current",
    )
    args = parser.parse_args()

    kb_root = Path(os.environ.get("KB_ROOT", Path.home() / "kb"))
    source_path = kb_root / "notes" / "essays" / "math-harness.md"
    target_path = Path(__file__).resolve().parent / "pages" / "math-harness.md"
    rendered = render(source_path.read_text(encoding="utf-8"))

    if args.check:
        current = target_path.read_text(encoding="utf-8") if target_path.exists() else ""
        if current != rendered:
            raise SystemExit(f"{target_path} is stale; rerun {Path(__file__).name}")
        return

    target_path.write_text(rendered, encoding="utf-8")


if __name__ == "__main__":
    main()
