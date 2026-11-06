-- A table's notes (::: {.table-notes}), set below the table in smaller, left-aligned type.
function Div(el)
  if not el.classes:includes("table-notes") or not quarto.doc.is_format("typst") then
    return nil
  end
  local blocks = { pandoc.RawBlock("typst",
    "#block(width: 100%, above: 0.6em)[#set text(size: 8pt)\n#set align(left)\n#set par(justify: false)") }
  for _, b in ipairs(el.content) do table.insert(blocks, b) end
  table.insert(blocks, pandoc.RawBlock("typst", "]"))
  return blocks
end
