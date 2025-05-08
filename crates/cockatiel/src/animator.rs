use crate::prelude::*;
use bevy::{prelude::*, utils::hashbrown::HashMap};
use if_chain::if_chain;
use std::time::Duration;
#[derive(Default)]
pub struct AnimatorPlugin<Tag: AnimatorTag> {
  _marker: std::marker::PhantomData<Tag>,
}
impl<Tag: AnimatorTag> Plugin for AnimatorPlugin<Tag> {
  fn build(&self, app: &mut App) {
    app
      .add_systems(
        Update,
        (execute_animations::<Tag>, sync_animations::<Tag>).chain(),
      )
      .add_event::<AnimationEvent<Tag::Event>>();
  }
}

#[derive(Clone, PartialEq, Eq, Debug, Reflect)]
struct Frame<E: AnimationEventPayload> {
  index: usize,
  duration: Duration,
  event: Option<E>,
}
impl<E: AnimationEventPayload> Frame<E> {
  pub fn new(index: usize, duration: Duration) -> Self {
    Self {
      index,
      duration,
      event: None,
    }
  }
}
#[derive(Clone, PartialEq, Eq, Debug, Reflect)]
pub struct FrameData<E: AnimationEventPayload> {
  frames: Vec<Frame<E>>,
  loops: bool,
}
impl<E: AnimationEventPayload> FrameData<E> {
  #[allow(dead_code)]
  pub fn new(frames: Vec<(usize, f32)>, loops: bool) -> Self {
    let frames: Vec<Frame<_>> = frames
      .into_iter()
      .map(|(index, seconds)| {
        let duration = Duration::from_secs_f32(seconds);
        Frame::new(index, duration)
      })
      .collect();

    Self { frames, loops }
  }

  pub fn homogenous(frame_count: usize, offset: usize, fps: u8, loops: bool) -> Self {
    let duration = Duration::from_secs_f32(1.0 / (fps as f32));
    let frames: Vec<Frame<_>> = (0..frame_count)
      .map(|i| Frame::new(i + offset, duration))
      .collect();

    Self { frames, loops }
  }
}

#[derive(Component, Debug, Reflect, Clone)]
pub enum Animation<E: AnimationEventPayload> {
  NonDirectional(FrameData<E>),
  BiDirectional {
    up: FrameData<E>,
    down: FrameData<E>,
  },
}

impl<E: AnimationEventPayload> Animation<E> {
  #[allow(dead_code)]
  pub fn non_directional(animation: FrameData<E>) -> Self {
    Self::NonDirectional(animation)
  }
  pub fn bi_directional(up: FrameData<E>, down: FrameData<E>) -> Self {
    Self::BiDirectional { up, down }
  }

  fn get(&self, direction: Option<&LookDirection>) -> Option<&FrameData<E>> {
    match (self, direction) {
      (Animation::NonDirectional(frame_data), _) => Some(frame_data),
      (Animation::BiDirectional { up, down }, Some(direction)) => match direction {
        LookDirection::UpLeft | LookDirection::UpRight => Some(up),
        LookDirection::DownLeft | LookDirection::DownRight => Some(down),
      },
      (_, _) => None,
    }
  }

  pub fn event_at(self, index: usize, event: E) -> Self {
    match self {
      Self::NonDirectional(mut framedata) => {
        framedata.frames[index].event = Some(event);
        Self::NonDirectional(framedata)
      }
      Self::BiDirectional { mut up, mut down } => {
        up.frames[index].event = Some(event.clone());
        down.frames[index].event = Some(event);
        Self::BiDirectional { up, down }
      }
    }
  }

  pub fn flip_x(&self, direction: Option<&LookDirection>) -> bool {
    match (self, direction) {
      (Animation::BiDirectional { up: _, down: _ }, Some(direction)) => match direction {
        LookDirection::UpRight | LookDirection::DownRight => false,
        LookDirection::DownLeft | LookDirection::UpLeft => true,
      },
      (_, _) => false,
    }
  }
}

#[derive(Reflect)]
pub struct ConditionalAnimation<Tag: AnimatorTag> {
  animation: Animation<Tag::Event>,
  #[allow(clippy::type_complexity)]
  predicate: Box<dyn Fn(&Tag::Input) -> bool + Send + Sync>,
}
impl<Tag: AnimatorTag> ConditionalAnimation<Tag> {
  pub fn new<F>(condition: F, animation: Animation<Tag::Event>) -> Self
  where
    F: Fn(&Tag::Input) -> bool + Send + Sync + 'static,
  {
    Self {
      animation,
      predicate: Box::new(condition),
    }
  }
}
impl<Tag: AnimatorTag> std::fmt::Debug for ConditionalAnimation<Tag> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("ConditionalAnimation")
      .field("animation", &self.animation)
      .field("condition", &"<function>")
      .finish()
  }
}
#[derive(Debug, Reflect)]
pub struct AnimationGroup<Tag: AnimatorTag> {
  conditional_animations: Vec<ConditionalAnimation<Tag>>,
  base_animation: Option<Animation<Tag::Event>>,
}
impl<Tag: AnimatorTag> Default for AnimationGroup<Tag> {
  fn default() -> Self {
    Self {
      conditional_animations: vec![],
      base_animation: None,
    }
  }
}
impl<Tag: AnimatorTag> AnimationGroup<Tag> {
  pub fn when<F>(mut self, condition: F, animation: Animation<Tag::Event>) -> Self
  where
    F: Fn(&Tag::Input) -> bool + 'static + Send + Sync,
  {
    self
      .conditional_animations
      .push(ConditionalAnimation::new(condition, animation));
    self
  }
  pub fn otherwise(mut self, animation: Animation<Tag::Event>) -> Self {
    self.base_animation = Some(animation);
    self
  }
  fn choose(&self, inputs: &Tag::Input) -> Option<&Animation<Tag::Event>> {
    for ConditionalAnimation {
      animation,
      predicate: condition,
    } in self.conditional_animations.iter()
    {
      if condition(inputs) {
        return Some(animation);
      }
    }
    if let Some(ref base_animation) = self.base_animation {
      return Some(base_animation);
    }
    None
  }
}
impl<Tag: AnimatorTag> From<Animation<Tag::Event>> for AnimationGroup<Tag> {
  fn from(value: Animation<Tag::Event>) -> Self {
    Self {
      conditional_animations: vec![],
      base_animation: Some(value),
    }
  }
}

pub fn group<Tag: AnimatorTag>(_tag: Tag) -> AnimationGroup<Tag> {
  AnimationGroup::default()
}
#[derive(Component, Debug, Reflect)]
pub struct Animator<Tag: AnimatorTag> {
  timer: Timer,
  frame_index: usize,
  pub inputs: Tag::Input,
  next_shift: Option<Tag::Shift>,
  animations: HashMap<Tag::State, AnimationGroup<Tag>>,
  state_machine: Machine<Tag::Input, Tag::State, Tag::Shift>,
}
pub trait AnimatorTag: 'static + Send + Sync {
  type Input: AnimationInput;
  type State: AnimationState;
  type Event: AnimationEventPayload;
  type Shift: AnimationShift;

  fn transitions() -> Vec<Transition<Self::State, Self::Input, Self::Shift>>;
  fn animations() -> HashMap<Self::State, impl Into<AnimationGroup<Self>>>
  where
    Self: Sized;
}

#[derive(Event)]
pub struct AnimationEvent<E: AnimationEventPayload> {
  pub entity: Entity,
  pub event: E,
}

// in the same crate as trait, you can implement trait for anything you want
// in the same crate as defining a struct or enum, you can implement any trait for that struct or enum
impl<Tag: AnimatorTag> Default for Animator<Tag> {
  fn default() -> Self {
    let animations = Tag::animations()
      .into_iter()
      .map(|(key, group)| (key, group.into()))
      .collect::<HashMap<Tag::State, AnimationGroup<Tag>>>();
    if animations.is_empty() {
      panic!("Animator must have at least one animation");
    }
    let transitions = Tag::transitions();
    Self {
      timer: Timer::new(Duration::from_secs_f32(0.0), TimerMode::Once),
      animations,
      state_machine: Machine::new(transitions),
      frame_index: 0,
      next_shift: None,
      inputs: Tag::Input::default(),
    }
  }
}

impl<Tag: AnimatorTag> Animator<Tag> {
  pub fn new() -> Self {
    Self::default()
  }
  pub fn with_inputs(inputs: Tag::Input) -> Self {
    let mut new = Self::new();
    new.inputs = inputs;
    new
  }

  fn reset(&mut self, atlas: &mut TextureAtlas, frame_data: &FrameData<Tag::Event>) {
    self.frame_index = 0;
    self.start_timer(frame_data);
    atlas.index = frame_data
      .frames
      .first()
      .expect("no frame to reset to")
      .index;
  }

  fn advance_frame_index(&mut self, frame_data: &FrameData<Tag::Event>) {
    use std::cmp::min;

    let frame_amount = frame_data.frames.len();
    self.frame_index = if frame_data.loops {
      (self.frame_index + 1) % frame_amount
    } else {
      min(self.frame_index + 1, frame_amount - 1)
    };
  }

  fn get_current_frame<'a>(&self, frame_data: &'a FrameData<Tag::Event>) -> &'a Frame<Tag::Event> {
    frame_data
      .frames
      .get(self.frame_index)
      .expect("frame_index was out of bounds when advancing")
  }

  fn next<'a>(&mut self, frame_data: &'a FrameData<Tag::Event>) -> &'a Frame<Tag::Event> {
    self.advance_frame_index(frame_data);
    self.get_current_frame(frame_data)
  }

  fn start_timer(&mut self, frame_data: &FrameData<Tag::Event>) {
    self.timer = Timer::new(
      frame_data
        .frames
        .get(self.frame_index)
        .expect("frame_index was out of bounds when starting timer")
        .duration,
      TimerMode::Once,
    );
  }

  fn get_animation(&self) -> Option<&Animation<Tag::Event>> {
    //SAFE: animation_index can only be changed via `change_animation`, and that checks that it is inbounds
    let animation_group = self.animations.get(self.state_machine.current_state())?;
    animation_group.choose(&self.inputs)
  }

  fn get_frames(&self, direction: Option<&LookDirection>) -> Option<FrameData<Tag::Event>> {
    let animation = self.get_animation()?;
    animation.get(direction).cloned()
  }

  pub fn shift(&mut self, shift: Tag::Shift) {
    match (&self.next_shift, shift) {
      (None, shift) => self.next_shift = Some(shift),
      (Some(old_shift), new_shift) if new_shift > *old_shift => self.next_shift = Some(new_shift),
      _ => {}
    }
  }
}

pub fn execute_animations<Tag: AnimatorTag>(
  time: Res<Time>,
  mut query: Query<(
    Entity,
    &mut Animator<Tag>,
    &mut Sprite,
    Option<&LookDirection>,
  )>,
  mut event_writer: EventWriter<AnimationEvent<Tag::Event>>,
) {
  for (entity, mut animator, mut sprite, direction) in &mut query {
    if let Some(atlas) = &mut sprite.texture_atlas {
      let inputs = animator.inputs.clone();
      let old_state = animator.state_machine.current_state().clone();

      // Required for animations which need to play fully before transitioning
      let is_last_frame = animator
        .get_frames(direction)
        .map(|f| f.frames.len() - 1 == animator.frame_index)
        .unwrap_or(true);
      let shift = animator.next_shift.take();
      let step_result = animator.state_machine.step(&inputs, is_last_frame, shift);
      let frame_data = animator.get_frames(direction).clone();
      if let Some(frame_data) = frame_data {
        if step_result.changed {
          debug!(
            "Animation changed from {:?} to {:?} with inputs: {:?} (step_result: {:?})",
            old_state,
            animator.state_machine.current_state(),
            inputs,
            step_result
          );
          animator.reset(atlas, &frame_data);
        }

        animator.timer.tick(time.delta());

        if animator.timer.just_finished() {
          let frame = animator.next(&frame_data);
          atlas.index = frame.index;

          if let Some(animation) = animator.get_animation() {
            sprite.flip_x = animation.flip_x(direction);
          }

          if let Some(ref event) = frame.event {
            let animator_event = AnimationEvent {
              entity,
              event: event.clone(),
            };
            event_writer.send(animator_event);
          }

          animator.start_timer(&frame_data);
        }
      }
    }
  }
}

#[derive(Component)]
pub struct DerivedAnimator<Tag: AnimatorTag> {
  pub animations: HashMap<Tag::State, Animation<Tag::Event>>,
  animator_target: Entity,
}
impl<Tag: AnimatorTag> DerivedAnimator<Tag> {
  pub fn new(
    animations: HashMap<Tag::State, Animation<Tag::Event>>,
    animator_target: Entity,
  ) -> Self {
    Self {
      animations,
      animator_target,
    }
  }
}

pub fn sync_animations<Tag: AnimatorTag>(
  sources: Query<(&Animator<Tag>, Option<&LookDirection>)>,
  mut targets: Query<(&mut Sprite, &mut DerivedAnimator<Tag>)>,
) {
  for (mut sprite, target) in &mut targets {
    let Ok((source, direction)) = sources.get(target.animator_target) else {
      continue;
    };

    let current_state = source.state_machine.current_state();
    if_chain! {
      if let Some(animation) = target.animations.get(current_state);
      if let Some(frame_data) = animation.get(direction);
      if let Some(frame) = frame_data.frames.get(source.frame_index);
      then {
        sprite.flip_x = animation.flip_x(direction);
        if let Some(atlas) = &mut sprite.texture_atlas {
            atlas.index = frame.index;
        }
      }
    }
  }
}

#[derive(Component, Default, Reflect)]
pub enum LookDirection {
  UpRight,
  #[default]
  DownRight,
  DownLeft,
  UpLeft,
}
impl From<Vec2> for LookDirection {
  fn from(value: Vec2) -> Self {
    match (value.x, value.y) {
      (x, y) if x >= 0.0 && y >= 0.0 => LookDirection::UpRight,
      (x, y) if x < 0.0 && y >= 0.0 => LookDirection::UpLeft,
      (x, y) if x < 0.0 && y < 0.0 => LookDirection::DownLeft,
      (x, y) if x >= 0.0 && y < 0.0 => LookDirection::DownRight,
      (_, _) => panic!("Could not parse Vec2 {value:?} into LookDirection!"),
    }
  }
}
impl From<Vec3> for LookDirection {
  fn from(value: Vec3) -> Self {
    Self::from(value.truncate())
  }
}
