# Local Fonts

## Adding Custom Fonts

To add custom fonts not available in Google Fonts (like Data70, OCR-A, OCR-B, etc.):

### 1. Add font files here

Place your font files in this directory:
- `.ttf` (TrueType)
- `.woff` (Web Open Font Format)
- `.woff2` (Web Open Font Format 2 - recommended)

Example:
```
/styles/fonts/
  data70.woff2
  data70.woff
  ocr-a.woff2
```

### 2. Add font configuration to helpers

In `R/typewriter.R` and `R/rotateTypewriter.R`, add to the font switch:

```r
"data70" = list(
  family = "'Data70', monospace",
  link = NULL,  # NULL means it's a local font
  local = "data70"  # name of the font file (without extension)
),
```

### 3. Font face CSS is auto-generated

When `link` is `NULL`, the helper will generate:

```css
@font-face {
  font-family: 'Data70';
  src: url('psha/styles/fonts/data70.woff2') format('woff2'),
       url('psha/styles/fonts/data70.woff') format('woff');
  font-weight: normal;
  font-style: normal;
}
```

## Font Sources

### Free OCR-Style Fonts
- **OCR-A** (license required)
- **OCR-B** (license required)  
- **Data70** (free alternative to OCR)
- Download from: https://www.dafont.com/data70.font

### Free Vintage Computer Fonts
- **Print Char 21** (dot matrix printer)
- **FF Dot Matrix** (grid-based)
- Download from: https://www.1001fonts.com/

### Converting Fonts
If you have `.ttf` files, convert to `.woff2` for better web performance:
```bash
# Using fonttools
pip install fonttools brotli
pyftsubset font.ttf --output-file=font.woff2 --flavor=woff2
```

## Usage

Once configured:

```r
showTypewriter(
  text = "SYSTEM READY",
  font = "data70",
  color = "#ff0000"
)
```
