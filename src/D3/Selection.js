import * as d3 from 'd3';

export const _append = selection => element => () =>  selection.append(element)

export const _bindData = datum => selection => () =>  selection.data(datum)

export const _call = selection => fn => () =>  selection.call(fn)

export const _mapSelectionLoop = updateData => selection => () => selection.data(function (d) { return updateData(d) });

export const _mapSelectionLoopIndexed = updateData => selection => () => selection.data(function (d, i) { return updateData(d)(i) });

export const _empty = selection => () => selection.empty()

export const _enter = selection => () => selection.enter()

export const _filter = cond => selection => () => selection.filter(cond)

export const _node = selection => just => nothing => () => {
  const result = selection.node()
  if (result === null)
    return nothing 
  else 
    return just(result)
}

export const _remove = selection => () => selection.remove()

export const _select = selector => () => d3.select(selector);

export const _selectAll = selector => () => d3.selectAll(selector)

export const _selectAllFromSelection = selector => selection => () => selection.selectAll(selector)

export const _selectFromSelection = selector => selection => () => selection.select(selector)

export const _transition = selection => () => selection.transition()

export const _getData = selection => () => selection.data()
