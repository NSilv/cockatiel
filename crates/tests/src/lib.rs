mod test {
  use bevy::prelude::*;
  use cockatiel::prelude::*;
  use hashbrown::HashMap;

  #[macro_export]
  macro_rules! hashmap {
    {$($key:expr => $value:expr),*} => {
        {
            let mut map = HashMap::new();
            $(map.insert($key, $value);)*
            map
        }
    };
  }

  #[derive(Default)]
  pub struct CursorAnimations;

  #[animation_state]
  #[derive(PartialEq, Eq, Hash, Clone, Debug)]
  pub enum State {
    Hidden,
    Cursor,
    Target,
  }

  #[derive(AnimationInput, Debug, Clone, Default)]
  pub struct Inputs {
    pub is_hidden: bool,
    pub is_target: bool,
  }

  #[derive(AnimationEventPayload, Event, Clone, Debug)]
  pub enum Events {}

  #[derive(AnimationShift, Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
  #[allow(dead_code)]
  pub enum Shift {}

  impl AnimatorTag for CursorAnimations {
    type Input = Inputs;
    type State = State;
    type Event = Events;
    type Shift = Shift;

    // fn log() -> Option<String> {
    //     Some("Cursor".to_string())
    // }

    fn transitions() -> Vec<Transition<State, Inputs, Shift>> {
      transitions![
          Enter -> Hidden when is_hidden,
          Enter -> Cursor when !is_hidden,
          Cursor -> Target when is_target,
          Target -> Cursor when and(!is_target, and(is_hidden, !is_target)),
          Hidden -> Cursor when !is_hidden,
          Cursor -> Exit when is_hidden,
          Target -> Exit when is_hidden,
      ]
    }

    fn animations() -> HashMap<Self::State, impl Into<AnimationGroup<Self>>> {
      let hidden = Animation::non_directional(FrameData::new(vec![(0, 0.05)], false));
      let cursor = Animation::non_directional(FrameData::new(vec![(1, 0.05)], false));
      let target = Animation::non_directional(FrameData::new(vec![(2, 0.05)], true));

      hashmap! {
          State::Hidden => hidden,
          State::Cursor => cursor,
          State::Target => target
      }
    }
  }
  #[test]
  fn test_macro() {}
}
