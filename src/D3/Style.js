export const _setStyleConst = style => value => selection => () => selection.style(style, value);

export const _setStyle = style => toValue => selection => () =>
  selection.style(style, (d) => toValue(d));

export const _setStyleI = style => toValue => selection => () => selection.style(style, (d, i) => toValue(d)(i));
