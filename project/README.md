# UiLayoutLang

UiLayoutLang is a small declarative layout language for nested UI rectangles. It parses a window declaration, resolves a single layout tree to absolute pixel coordinates, and can optionally render the result as SVG.

## Build And Test

From this directory:

```powershell
cabal test
```

Run the demo program:

```powershell
cabal run ui-layout-lang -- --demo
```

Run a layout file:

```powershell
cabal run ui-layout-lang -- path\to\layout.ui
```

Write an SVG:

```powershell
cabal run ui-layout-lang -- path\to\layout.ui --svg out.svg
```

This project uses only `base`. Cabal is the only required project build tool.

If Haskell Language Server reports that it cannot find modules such as `UiLayoutLang.Types`, open the repository root or the `project/` directory in the IDE and reload HLS. The root `hie.yaml` maps `project/src`, `project/app`, and `project/test` to the correct Cabal components.

## Language

Example:

```text
window "Main" 800 x 600 {
  row {
    box { width: 20%, height: 100%, color: red  }
    box { width: 80%, height: 100%, color: blue }
  }
}
```

Supported forms:

```text
window "Title" WIDTH x HEIGHT { LAYOUT }

LAYOUT =
  row { PROPERTIES_AND_CHILDREN }
  col { PROPERTIES_AND_CHILDREN }
  box { PROPERTIES }

PROPERTY =
  width: SIZE
  height: SIZE
  color: name
  color: "#rrggbb"

SIZE =
  auto
  120
  120px
  50%
```

Comments are supported with `#`, `//`, and `/* ... */`.

## Design Choices

The spec leaves a few behaviors open. These are the choices used here:

- A `window` contains exactly one root layout. This keeps the output as one resolved tree.
- `row` and `col` are containers. They may have `width`, `height`, and `color`, plus child layouts.
- `box` is a leaf. If a `box` contains a child layout, parsing fails and the error points at the box.
- Missing `width` or `height` means `auto`.
- On the cross axis, `auto` fills the parent.
- On the main axis, `auto` children share remaining space after fixed pixel and percentage children are accounted for. Remainder pixels go to earlier auto children.
- If fixed and percentage children use less than the parent, they keep their requested sizes and unused space stays at the end of the row or column.
- Overflow is clipped sequentially along the layout axis. Each child receives at most the remaining parent space, so no child sticks out of its parent.
- Percentage sizes are limited to `0%` through `100%` and are floored to whole pixels.
- The root layout is sized from its own width and height. With the default `auto`, it fills the window. Explicit root sizes are clamped to the window.
- The SVG renderer draws every resolved node. Nodes without a color are shown as light gray with low opacity so container bounds remain visible.

## Test Coverage

The test executable covers:

- parser success and syntax-error cases
- comment handling
- row percentage splitting
- auto-size distribution
- overflow clipping
- the example layout's computed coordinates
- generated layout invariants that every child stays inside its parent and child sizes do not exceed the parent along the layout axis
