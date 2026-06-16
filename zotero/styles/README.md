# CSL styles

Local CSL style directory for Org Cite/Zotero-compatible exports.

Upstream repository: https://github.com/citation-style-language/styles

Raw download base:

```sh
BASE=https://raw.githubusercontent.com/citation-style-language/styles/master
```

Downloaded locally:

```sh
curl -L -o apa.csl "$BASE/apa.csl"
curl -L -o aip.csl "$BASE/american-institute-of-physics.csl"
curl -L -o acs.csl "$BASE/american-chemical-society.csl"
curl -L -o ieee.csl "$BASE/ieee.csl"
```

Note: I did not find a dedicated MIT / Massachusetts Institute of Technology
style in the official CSL repository. The closest filename match is RMIT
University Harvard, saved here as `rmit.csl`.

Other popular styles:

```sh
curl -L -o rmit.csl "$BASE/rmit-university-harvard.csl"
curl -L -o chicago-author-date.csl "$BASE/chicago-author-date.csl"
curl -L -o chicago-fullnote-bibliography.csl "$BASE/chicago-fullnote-bibliography.csl"
curl -L -o mla.csl "$BASE/modern-language-association.csl"
curl -L -o vancouver.csl "$BASE/vancouver.csl"
curl -L -o nature.csl "$BASE/nature.csl"
curl -L -o ama.csl "$BASE/american-medical-association.csl"
curl -L -o elsevier-harvard.csl "$BASE/elsevier-harvard.csl"
curl -L -o harvard-cite-them-right.csl "$BASE/harvard-cite-them-right.csl"
```

Use one in an Org file with:

```org
#+cite_export: csl apa.csl
```
