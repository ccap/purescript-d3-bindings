import * as d3 from 'd3';

export const _attachScaleBottom = scale => selection => () => 
  selection.call(d3.axisBottom(scale))

export const _attachScaleLeft = scale => selection => () => 
  selection.call(d3.axisLeft(scale))

export const _bandwidth = scale => scale.bandwidth();

export const _scaleBand = ({ domain, range }) => () => 
  d3.scaleBand().domain(domain).range(range)

export const _scaleBandValue = scale => number => scale(number);

export const _scaleLinear = ({ domain, range }) => () => 
  d3.scaleLinear().domain(domain).range(range)

export const _scaleLinearValue = scale => number => scale(number);

// Axis 
export const _attachAxis = axis => selection => () => selection.call(axis);

export const _axisLeft = scale => () => d3.axisLeft(scale);

export const _axisBottom = scale => () => d3.axisBottom(scale);

// Setters 
export const _setPadding = scale => padding => () => scale.padding(padding);

export const _setTicks = scale => ticks => () => scale.ticks(ticks);

export const _setTickValues = scale => tickValues => () => scale.tickValues(tickValues);

export const _setTickFormat = scale => tickFormat => () => scale.tickFormat((d, i) =>  tickFormat(d)(i));

// TODO (Inner/Outer distinction)
export const _setTickSize = scale => tickSize => () => scale.tickSizeInner(tickSize).tickSizeOuter(0);

