
export const _setTextConst = value => selection => () =>
  selection.text(value);

export const _setText = toValue => selection => () =>
  selection.text((d) => toValue(d));

export const _setTextI = toValue => selection => () =>
  selection.text((d, i) => toValue(d)(i));
