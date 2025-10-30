# Install Data70 Font

## Download

1. Go to: https://www.dafont.com/data70.font
2. Click "Download" button
3. Extract the .ttf file from the ZIP
4. Place `data70.ttf` in this directory

## Convert to WOFF2 (optional but recommended)

```bash
cd /Users/averrik/Cloud/github/psha/styles/fonts

# If you have fonttools installed:
pip3 install fonttools brotli
pyftsubset data70.ttf --output-file=data70.woff2 --flavor=woff2

# Or use online converter:
# https://cloudconvert.com/ttf-to-woff2
```

## Alternative: OCR-like fonts

If Data70 is not available, these alternatives work well:
- **Overpass Mono** (already configured) - blocky OCR style
- **Courier Prime** (already configured) - typewriter style
- **Nova Mono** - geometric OCR-like
