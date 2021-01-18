type coord_t = {
  x: int,
  y: int,
  value: int,
};

type direction_t =
  | LEFT
  | RIGHT
  | UP
  | DOWN;

type step_t = {
  step: int,
  direction: direction_t,
  remain: int,
};

let startCoord = {x: 0, y: 0, value: 1};
let startStep = {step: 1, direction: RIGHT, remain: 1};

let makeNextStep = step => {
  switch (step.remain == 1, step.direction) {
  | (false, _) => {...step, remain: step.remain - 1}
  | (true, RIGHT) => {...step, direction: UP, remain: step.step}
  | (true, UP) => {
      step: step.step + 1,
      direction: LEFT,
      remain: step.step + 1,
    }
  | (true, LEFT) => {...step, direction: DOWN, remain: step.step}
  | (true, DOWN) => {
      step: step.step + 1,
      direction: RIGHT,
      remain: step.step + 1,
    }
  };
};

let rec findMe = (target, coord, step) => {
  let nextCoord =
    switch (step.direction) {
    | RIGHT => {...coord, x: coord.x + 1, value: coord.value + 1}
    | UP => {...coord, y: coord.y + 1, value: coord.value + 1}
    | LEFT => {...coord, x: coord.x - 1, value: coord.value + 1}
    | DOWN => {...coord, y: coord.y - 1, value: coord.value + 1}
    };

  let nextStep = step->makeNextStep;

  target == nextCoord.value ? nextCoord : target->findMe(nextCoord, nextStep);
};

let coordLength = coord => coord.x->Js.Math.abs_int + coord.y->Js.Math.abs_int;

12->findMe(startCoord, startStep)->coordLength->Js.log;
23->findMe(startCoord, startStep)->coordLength->Js.log;
1024->findMe(startCoord, startStep)->coordLength->Js.log;
265149->findMe(startCoord, startStep)->coordLength->Js.log;

let rec findMe2 = (target, coord, step, allValues) => {
  let nextCoord =
    switch (step.direction) {
    | RIGHT => {...coord, x: coord.x + 1}
    | UP => {...coord, y: coord.y + 1}
    | LEFT => {...coord, x: coord.x - 1}
    | DOWN => {...coord, y: coord.y - 1}
    };

  let nextValue =
    allValues
    ->Belt.List.keep(coord => {
        switch (
          Js.Math.abs_int(nextCoord.x - coord.x),
          Js.Math.abs_int(nextCoord.y - coord.y),
        ) {
        | (1, 0)
        | (1, 1)
        | (0, 1) => true
        | _ => false
        }
      })
    ->Belt.List.map(coord => coord.value)
    ->Belt.List.reduce(0, (+));

  let nextStep = step->makeNextStep;

  let nextCoord = {...nextCoord, value: nextValue};
  // nextCoord->Js.log;
  let nextAllValues = [nextCoord, ...allValues];

  target < nextValue
    ? nextValue : target->findMe2(nextCoord, nextStep, nextAllValues);
};

4->findMe2(startCoord, startStep, [startCoord])->Js.log;
363->findMe2(startCoord, startStep, [startCoord])->Js.log;
265149->findMe2(startCoord, startStep, [startCoord])->Js.log;