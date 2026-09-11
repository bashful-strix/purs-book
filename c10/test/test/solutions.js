// ex 1 {{{

export function volumeFn(l, w, h) {
  return l * w * h;
}

export const volumeArrow = l => w => h => l * w * h;

// }}}

// ex 2 {{{

export const cumulativeSumsComplex = xs => {
  let sum = { real: 0, imag: 0 };
  let sums = [];
  xs.forEach(x => {
    sum = { real: sum.real + x.real, imag: sum.imag + x.imag };
    sums.push(sum);
  });
  return sums;
};

// }}}
