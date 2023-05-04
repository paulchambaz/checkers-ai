#let project(
  title: "",
  authors: (),
  abstract: [],
  doc,
) = {

  set text(
    font: "New Computer Modern",
    size: 11pt
  )

  set page(
    paper: "a4",
    margin: (x: 2cm, y: 3cm),
    numbering: "1",
  )


  set align(center)
  set heading(
    numbering: "1.",
  )

  text(17pt, strong(smallcaps(title)))

  v(8pt)

  let count = authors.len()
  let ncols = calc.min(count, 3)
  grid(
    columns: (1fr,) * ncols,
    row-gutter: 24pt,
    ..authors.map(author => [
      *#author.name* \
      #link("mailto:" + author.email)
    ]),
  )

  v(4pt)

  par(justify: false)[
    *Abstract*
    #v(4pt)
    #abstract
  ]


  v(8pt)

  line(start: (2%,0%), end: (98%, 0%), stroke: 0.4pt)

  v(8pt)

  set align(left)
  set par(justify: true)
  columns(2, doc)
}
