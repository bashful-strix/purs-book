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

// ex 3 {{{

// cheat for this one - not worth it
export const quadraticRootsImpl = pair => ({ a, b, c }) => {
  const radicand = b * b - 4 * a * c;
  if (radicand >= 0) {
    const rt = Math.sqrt(radicand);
    return pair
      ({ real: (-b + rt) / (2 * a), imag: 0 })
      ({ real: (-b - rt) / (2 * a), imag: 0 });
  } else {
    const rt = Math.sqrt(-radicand);
    return pair
      ({ real: -b / (2 * a), imag: rt / (2 * a) })
      ({ real: -b / (2 * a), imag: -rt / (2 * a) });
  }
};

export const toMaybeIml = just => nothing => a =>
  a === undefined ? nothing : just(a);

// }}}

// ex 4 }}}

export const valuesOfMapImpl = a =>
  Array.from(new Map(a).values());

export const quadraticRootsJsonImpl = ({ a, b, c }) => {
  const radicand = b * b - 4 * a * c;
  if (radicand >= 0) {
    const rt = Math.sqrt(radicand);
    return [ { real: (-b + rt) / (2 * a), imag: 0 }
           , { real: (-b - rt) / (2 * a), imag: 0 }
           ];
  } else {
    const rt = Math.sqrt(-radicand);
    return [ { real: -b / (2 * a), imag: rt / (2 * a) }
           , { real: -b / (2 * a), imag: -rt / (2 * a) }
           ];
  }
};

// }}}
