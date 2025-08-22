/*

animation! {
  states = [idle, move, hurt]
  animations = [idle_animation, move_animation, hurt_animation]
  inputs = [is_moving : bool, is_hurt : bool]
  transitions {
    enter -> idle
    idle -> move, when: is_moving, wait: true
    any -> hurt, when: is_hurt
  }
}
*/

use std::hash::Hash;

use bevy_log::{debug, info};
use hashbrown::HashMap;

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum InputValue {
  Boolean(bool),
  Float(f32),
}
pub trait AnimationInput: Clone + Default + std::fmt::Debug + Send + Sync {
  type Vars: Clone + std::fmt::Debug + Send + Sync;
  fn get(&self, var: &Self::Vars) -> InputValue;
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum Condition<Input: AnimationInput> {
  IsTrue(<Input as AnimationInput>::Vars),
  IsFalse(<Input as AnimationInput>::Vars),
  And(Box<Condition<Input>>, Box<Condition<Input>>),
}
impl<Input: AnimationInput> Condition<Input> {
  fn evaluate(&self, input: &Input) -> bool {
    match self {
      Condition::IsTrue(var) => match input.get(var) {
        InputValue::Boolean(b) => b,
        _ => panic!("input value is not boolean"),
      },
      Condition::IsFalse(var) => match input.get(var) {
        InputValue::Boolean(b) => !b,
        _ => panic!("input value is not boolean"),
      },
      Condition::And(a, b) => a.evaluate(input) && b.evaluate(input),
    }
  }
}

pub trait AnimationState: Eq + Hash + Clone + std::fmt::Debug + Send + Sync {
  fn enter_state() -> Self;
  fn exit_state() -> Self;
  fn any_state() -> Self;
}

pub trait AnimationEventPayload: Clone + std::fmt::Debug + Send + Sync {}
#[derive(Clone, Debug)]
pub struct Transition<State: AnimationState, Inputs: AnimationInput, Shift: AnimationShift> {
  from: State,
  to: State,
  wait_until_finished: bool,
  condition: Option<Condition<Inputs>>,
  shift: Option<Shift>,
}
impl<State: AnimationState, Inputs: AnimationInput, Shift: AnimationShift>
  Transition<State, Inputs, Shift>
{
  pub fn new(
    from: State,
    to: State,
    wait_until_finished: bool,
    condition: Option<Condition<Inputs>>,
    shift: Option<Shift>,
  ) -> Self {
    Self {
      from,
      to,
      wait_until_finished,
      condition,
      shift,
    }
  }
}
pub trait AnimationShift: std::fmt::Debug + Send + Sync + Ord + Clone + Hash + Copy {}

#[derive(Debug)]
pub struct Machine<Input: AnimationInput, State: AnimationState, Shift: AnimationShift> {
  current_state: State,
  transitions: HashMap<State, Vec<Transition<State, Input, Shift>>>,
  transitions_from_any: Vec<Transition<State, Input, Shift>>, // transitions: HashMap<u8, u8>,
  transitions_from_shift: HashMap<Shift, Transition<State, Input, Shift>>,
  log: Option<String>,
}
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct AnimationStepResult<State: AnimationState> {
  pub state: State,
  pub changed: bool,
}

impl<Inputs: AnimationInput, State: AnimationState, Shift: AnimationShift>
  Machine<Inputs, State, Shift>
{
  pub fn new(transitions: Vec<Transition<State, Inputs, Shift>>, log: Option<String>) -> Self {
    let (transitions, transitions_from_any, transitions_from_shift) = transitions.into_iter().fold(
      (HashMap::new(), vec![], HashMap::new()),
      |(mut map, mut anys, mut shifts), transition| {
        Self::split_transition(transition, &mut map, &mut anys, &mut shifts);
        (map, anys, shifts)
      },
    );
    Self {
      current_state: State::enter_state(),
      transitions,
      transitions_from_any,
      transitions_from_shift,
      log,
    }
  }
  fn split_transition(
    transition: Transition<State, Inputs, Shift>,
    transitions: &mut HashMap<State, Vec<Transition<State, Inputs, Shift>>>,
    any_transitions: &mut Vec<Transition<State, Inputs, Shift>>,
    shift_transitions: &mut HashMap<Shift, Transition<State, Inputs, Shift>>,
  ) {
    match &transition {
      Transition {
        shift: Some(shift), ..
      } => {
        shift_transitions.insert(*shift, transition);
      }
      Transition { to, .. } if to == &State::any_state() => {
        any_transitions.push(transition);
      }
      _ => transitions
        .entry(transition.from.clone())
        .or_default()
        .push(transition),
    };
  }
  pub fn current_state(&self) -> &State {
    &self.current_state
  }
  fn get_transitions_for(
    &mut self,
    state: &State,
    is_current_animation_finished: bool,
    shift: &Option<Shift>,
  ) -> Option<Vec<Transition<State, Inputs, Shift>>> {
    if_chain! {
      if let Some(shift) = shift;
      if let Some(transition) = self.transitions_from_shift.get(shift);
      then {
        return Some(vec![transition.clone()]);
      }
    }
    let transitions_for_any = self
      .transitions_from_any
      .iter()
      .filter(|t| t.to != *state)
      .cloned()
      .collect::<Vec<_>>();
    if let Some(transitions_for_state) = self.transitions.get_mut(state) {
      transitions_for_state.sort_by_key(|transition: &Transition<State, Inputs, Shift>| {
        match transition.condition.is_some() {
          true => 1,
          false => 0,
        }
      });
      /*
         | wait_until_finished | is_current_animation_finished | result |
         |---------------------|-------------------------------|--------|
         | true                | true                          | true   |
         | true                | false                         | false  |
         | false               | true                          | true   |
         | false               | false                         | true   |
      */
      transitions_for_state.extend(transitions_for_any);
      let transitions = transitions_for_state
        .iter()
        .filter(|t| !t.wait_until_finished || is_current_animation_finished)
        .cloned()
        .collect();
      Some(transitions)
    } else if !transitions_for_any.is_empty() {
      Some(transitions_for_any)
    } else {
      None
    }
  }

  pub fn step(
    &mut self,
    inputs: &Inputs,
    is_current_animation_finished: bool,
    next_shift: Option<Shift>,
  ) -> AnimationStepResult<State> {
    debug!("TODO: HANDLE {next_shift:?}");
    let current_state = self.current_state.clone();
    let mut changed = false;

    let Some(valid_transitions) =
      self.get_transitions_for(&current_state, is_current_animation_finished, &next_shift)
    else {
      return AnimationStepResult {
        state: current_state,
        changed,
      };
    };

    let mut transition = None;
    for t in valid_transitions {
      match &t.condition {
        None => {
          transition = Some(t);
          break;
        }
        Some(c) => {
          if c.evaluate(inputs) {
            transition = Some(t);
            break;
          }
        }
      }
    }

    if let Some(t) = transition {
      if let Some(name) = &self.log {
        info!(
          "[{name}] found transition: {t:?}. old_state = {:?}, new_state = {:?}",
          self.current_state,
          t.to.clone()
        );
      }
      self.current_state = t.to.clone();
      changed = true;
    }

    if self.current_state == State::exit_state() {
      self.current_state = State::enter_state();
      let mut result = self.step(inputs, is_current_animation_finished, next_shift);
      result.changed = true;
      result
    } else {
      AnimationStepResult {
        state: current_state,
        changed,
      }
    }
  }
}
#[macro_export]
macro_rules! transition {
  // no condition, no shift
  ($from:ident -> $to:ident) => {
    Transition::new(State::$from, State::$to, false, None, None)
  };
  ($from:ident |-> $to:ident) => {
    Transition::new(State::$from, State::$to, true, None, None)
  };

  // yes condition, no shift
  ($from:ident -> $to:ident when $condition:expr) => {
    Transition::new(State::$from, State::$to, false, Some($condition), None)
  };
  ($from:ident |-> $to:ident when $condition:expr) => {
    Transition::new(State::$from, State::$to, true, Some($condition), None)
  };

  // no condition, yes shift
  (-> $to:ident on $shift:expr) => {
    Transition::new(State::any_state(), State::$to, false, None, Some($shift))
  };
}
use hashbrown::HashMap;
use if_chain::if_chain;
pub use transition;
