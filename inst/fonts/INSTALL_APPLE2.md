# Install Apple II Fonts

## Authentic Apple II Fonts

### Print Char 21 (40 columns)
- **Download**: https://www.kreativekorp.com/software/fonts/apple2.shtml
- **Description**: Original Apple II 40-column font
- **Era**: 1977-1983
- **Best for**: Authentic Apple II experience

### PR Number 3 (80 columns)
- **Download**: https://www.kreativekorp.com/software/fonts/apple2.shtml  
- **Description**: Apple IIe 80-column card font
- **Era**: 1983+
- **Best for**: Apple IIe/IIc displays

### Apple II Screen Typeface (Modern Recreation)
- **Download**: https://www.kreativekorp.com/software/fonts/apple2.shtml
- **Description**: Modern vectorized version
- **Era**: Modern recreation
- **Best for**: High-resolution displays

## Already Available Alternative

**Press Start 2P** (already configured as `press`):
- Available in Google Fonts (no download needed)
- Very similar 8-bit aesthetic
- Works immediately with:

```r
showTypewriter(
  text = "]CATALOG\n]RUN HELLO",
  font = "press",
  fontSize = 0.6,  # Small for authentic pixelated look
  color = "#00ff00",  # Green phosphor
  bgColor = "#000000"
)
```

## Installation Steps for Local Fonts

1. **Download** from Kreative Korp website
2. **Extract** `.ttf` files
3. **Place in** `smartReports/inst/fonts/`
4. **Add configuration** in `typewriter.R`:

```r
"printchar21" = list(
  family = "'Print Char 21', monospace",
  link = NULL,
  local = "printchar21"
),
"prnumber3" = list(
  family = "'PR Number 3', monospace",
  link = NULL,
  local = "prnumber3"
)
```

5. **Use**:

```r
showTypewriter(
  text = "]LOAD GAME\n]RUN",
  font = "printchar21",
  color = "#00ff00"
)
```

## Apple II Color Recommendations

Classic Apple II colors:
- **Green phosphor**: `color = "#00ff00"`, `bgColor = "#000"`
- **White**: `color = "#ffffff"`, `bgColor = "#000"`
- **Color mode**: `color = "#ff00ff"` (magenta), `bgColor = "#000"`

## Note

For most use cases, **Press Start 2P** (font = "press") provides an excellent Apple II-style appearance without requiring font downloads.
