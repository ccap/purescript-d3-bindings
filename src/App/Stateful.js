
export const _offsetCoords = e => () => {
  const rect = e.currentTarget.getBoundingClientRect();
  const x = e.pageX - rect.left; 
  const y = e.pageY - rect.top;
  return { x, y }
}

export const _join = selection => () => {
  selection.join(
    enter => 
      enter.append("rect")
        .attr("x", d => d.x)
        .attr("y", d => d.y)
        .attr("height", 25.0)
        .attr("width", 25.0)
        .attr("fill", "red"),
    update => update
        .attr("x", d => d.x)
        .attr("y", d => d.y)
  )
}
