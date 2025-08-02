use crate::prelude::*;
use bevy::prelude::*;
use hashbrown::HashMap;
use if_chain::if_chain;
use std::{ops::Range, time::Duration};
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
    Self::range(offset..(frame_count + offset), fps, loops)
  }

  pub fn range(frames: Range<usize>, fps: u8, loops: bool) -> Self {
    let duration = Duration::from_secs_f32(1.0 / (fps as f32));
    let frames: Vec<Frame<_>> = frames.map(|i| Frame::new(i, duration)).collect();

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
  name: &'static str,
  animation: Animation<Tag::Event>,
  #[allow(clippy::type_complexity)]
  predicate: fn(&Tag::Input) -> bool,
}
impl<Tag: AnimatorTag> ConditionalAnimation<Tag> {
  pub fn new(predicate: fn(&Tag::Input) -> bool, animation: Animation<Tag::Event>) -> Self {
    Self {
      name: "",
      animation,
      predicate,
    }
  }
  pub fn new_named(
    name: &'static str,
    predicate: fn(&Tag::Input) -> bool,
    animation: Animation<Tag::Event>,
  ) -> Self {
    Self {
      name,
      animation,
      predicate,
    }
  }
}
impl<Tag: AnimatorTag> std::fmt::Debug for ConditionalAnimation<Tag> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("ConditionalAnimation")
      .field("name", &self.name)
      .field("animation", &self.animation)
      .field("condition", &"<function>")
      .finish()
  }
}

type VarOf<Tag> = <<Tag as AnimatorTag>::Input as AnimationInput>::Vars;
#[derive(Debug, Reflect)]
pub struct AnimationGroup<Tag: AnimatorTag> {
  conditional_animations: Vec<ConditionalAnimation<Tag>>,
  base_animation: Option<Animation<Tag::Event>>,
  speed_var: Option<VarOf<Tag>>,
}
impl<Tag: AnimatorTag> Default for AnimationGroup<Tag> {
  fn default() -> Self {
    Self {
      conditional_animations: vec![],
      base_animation: None,
      speed_var: None,
    }
  }
}
macro_rules! log {
  ($tag:ident, $msg:expr) => {
    if let Some(name) = $tag::log() {
      info!("[{name}] $msg")
    }
  };
}
impl<Tag: AnimatorTag> AnimationGroup<Tag> {
  pub fn speed(&self, input: &Tag::Input) -> f32 {
    match self.speed_var {
      None => 1.0,
      Some(ref var) => match input.get(var) {
        InputValue::Float(speed) => speed,
        InputValue::Boolean(_) => panic!("var {:?} shouldn't be a boolean", var),
      },
    }
  }
  pub fn with_speed(mut self, var: VarOf<Tag>) -> Self {
    self.speed_var = Some(var);
    self
  }
  pub fn when(
    mut self,
    predicate: fn(&Tag::Input) -> bool,
    animation: Animation<Tag::Event>,
  ) -> Self {
    self
      .conditional_animations
      .push(ConditionalAnimation::new(predicate, animation));
    self
  }
  pub fn when_named(
    mut self,
    name: &'static str,
    predicate: fn(&Tag::Input) -> bool,
    animation: Animation<Tag::Event>,
  ) -> Self
where {
    self
      .conditional_animations
      .push(ConditionalAnimation::new_named(name, predicate, animation));
    self
  }

  pub fn otherwise(mut self, animation: Animation<Tag::Event>) -> Self {
    self.base_animation = Some(animation);
    self
  }

  fn choose(&self, inputs: &Tag::Input) -> Option<&Animation<Tag::Event>> {
    // log!(Tag, "choosing with inputs: {inputs:?}");

    for ConditionalAnimation {
      name,
      animation,
      predicate,
    } in self.conditional_animations.iter()
    {
      // log!(Tag, "trying condition '{name}'");

      if predicate(inputs) {
        if let Some(tag_name) = Tag::log() {
          bevy::log::info!("[{tag_name}] AnimationGroup::choose - Chosen animation named '{name}'!")
        }
        return Some(animation);
      }
    }
    if let Some(ref base_animation) = self.base_animation {
      // log!(Tag, "AnimationGroup::choose - chosen base animation");
      return Some(base_animation);
    }
    // log!(Tag, "AnimationGroup::choose - no animation chosen");
    None
  }
}
impl<Tag: AnimatorTag> From<Animation<Tag::Event>> for AnimationGroup<Tag> {
  fn from(value: Animation<Tag::Event>) -> Self {
    Self {
      conditional_animations: vec![],
      base_animation: Some(value),
      speed_var: None,
    }
  }
}

#[derive(Debug)]
enum AnimationDirection {
  Paused,
  Forward,
  Backward,
}
impl From<f32> for AnimationDirection {
  fn from(value: f32) -> Self {
    if value == 0.0 {
      AnimationDirection::Paused
    } else if value > 0.0 {
      AnimationDirection::Forward
    } else {
      AnimationDirection::Backward
    }
  }
}
impl Into<i32> for &AnimationDirection {
  fn into(self) -> i32 {
    match self {
      AnimationDirection::Paused => 0,
      AnimationDirection::Forward => 1,
      AnimationDirection::Backward => -1,
    }
  }
}

#[derive(Debug, Reflect, Clone, Default, PartialEq)]
pub struct AnimationTimer {
  current: f32,
  duration: f32,
}
impl AnimationTimer {
  fn new(duration: f32) -> Self {
    Self::with_offset(duration, 0.0)
  }

  fn with_offset(duration: f32, offset: f32) -> Self {
    AnimationTimer {
      current: offset,
      duration,
    }
  }

  fn excess(&self) -> f32 {
    self.duration - self.current
  }

  fn tick(&mut self, time: f32) {
    self.current += time;
  }

  fn finished(&self) -> bool {
    self.current >= self.duration
  }
}

#[derive(Component, Debug, Reflect)]
pub struct Animator<Tag: AnimatorTag> {
  timer: AnimationTimer,
  frame_index: usize,
  pub inputs: Tag::Input,
  next_shift: Option<Tag::Shift>,
  animations: HashMap<Tag::State, AnimationGroup<Tag>>,
  state_machine: Machine<Tag::Input, Tag::State, Tag::Shift>,
}
impl<Tag: AnimatorTag> Animator<Tag> {
  fn get_speed(&self) -> f32 {
    self
      .get_animation_group()
      .map_or(1.0, |g| g.speed(&self.inputs))
  }
  fn is_last_frame(&self, look_dir: Option<&LookDirection>) -> bool {
    self
      .get_frames(look_dir)
      .map(|f| f.frames.len() - 1 == self.frame_index)
      .unwrap_or(true)
  }
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

  fn group() -> AnimationGroup<Self>
  where
    Self: Sized,
  {
    AnimationGroup::default()
  }

  fn log() -> Option<String> {
    None
  }
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
      timer: AnimationTimer::new(0.0),
      animations,
      state_machine: Machine::new(transitions, Tag::log()),
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
    atlas.index = frame_data
      .frames
      .first()
      .expect("no frame to reset to")
      .index;

    let duration = self.get_frame_duration(frame_data);
    self.timer = AnimationTimer::new(duration);
  }

  fn advance_frame_index(
    &mut self,
    frame_data: &FrameData<Tag::Event>,
    direction: &AnimationDirection,
  ) {
    let frame_amount = frame_data.frames.len() as i32;
    let old_frame_index = self.frame_index as i32;
    let direction: i32 = direction.into();

    let frame_index = if frame_data.loops {
      (old_frame_index + frame_amount + direction) % frame_amount
    } else {
      i32::clamp(old_frame_index + direction, 0, frame_amount - 1)
    };

    if let Some(name) = Tag::log() {
      info!("[{name}] old_frame_index = {old_frame_index}, new_frame_index = {frame_index}")
    }

    assert!(
      frame_index < frame_amount && frame_index >= 0,
      "0 <= frame_index[{frame_index}] < frame_amount[{frame_amount}] is false",
    );
    self.frame_index = frame_index as usize;
  }

  fn get_current_frame<'a>(&self, frame_data: &'a FrameData<Tag::Event>) -> &'a Frame<Tag::Event> {
    frame_data.frames.get(self.frame_index).unwrap_or_else(|| {
      panic!(
        "frame_index was out of bounds when advancing: {} in {:#?}",
        self.frame_index, frame_data.frames
      )
    })
  }

  fn next<'a>(
    &mut self,
    frame_data: &'a FrameData<Tag::Event>,
    direction: &AnimationDirection,
  ) -> &'a Frame<Tag::Event> {
    self.advance_frame_index(frame_data, direction);
    self.get_current_frame(frame_data)
  }

  fn get_frame_duration(&self, frame_data: &FrameData<Tag::Event>) -> f32 {
    frame_data
      .frames
      .get(self.frame_index)
      .expect("frame_index was out of bounds when starting timer")
      .duration
      .as_secs_f32()
  }

  fn get_animation_group(&self) -> Option<&AnimationGroup<Tag>> {
    self.animations.get(self.state_machine.current_state())
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
  for (entity, mut animator, mut sprite, look_dir) in &mut query {
    // Rotate the sprite based on look direction
    if let Some(animation) = animator.get_animation() {
      sprite.flip_x = animation.flip_x(look_dir);
    }

    if let Some(atlas) = &mut sprite.texture_atlas {
      // Ticking the animator timer
      let speed = animator.get_speed();
      let direction = &speed.into();
      animator.timer.tick(time.delta_secs() * speed.abs());

      if let Some(name) = Tag::log() {
        info!(
          "[{name}] ticking: {:?} {:?} by {}",
          animator.timer,
          direction,
          time.delta_secs() * speed.abs()
        )
      }

      // Stepping the animator state machine
      let inputs = animator.inputs.clone();
      let is_last_frame = animator.is_last_frame(look_dir);
      let has_frame_finished = animator.timer.finished();
      let shift = animator.next_shift.take();

      let step_result =
        animator
          .state_machine
          .step(&inputs, is_last_frame && has_frame_finished, shift);

      let frame_data = animator.get_frames(look_dir).clone();
      if let Some(frame_data) = frame_data {
        // Animator moved into a new state
        if step_result.changed {
          if let Some(tag_name) = Tag::log() {
            let current_state = animator.state_machine.current_state();
            info!("[{tag_name}] Animation state changed to {current_state:?} with inputs: {inputs:?} (step_result: {step_result:?}, look_dir: {look_dir:?})");
          }

          animator.reset(atlas, &frame_data);
        }

        // Animator has finished the frame
        if has_frame_finished {
          // Update sprite to match the new frame
          let frame = animator.next(&frame_data, direction);
          atlas.index = frame.index;

          // If the new frame has an associated event, send it
          if let Some(ref event) = frame.event {
            let animator_event = AnimationEvent {
              entity,
              event: event.clone(),
            };
            event_writer.send(animator_event);
          }

          // Start the timer for the new frame
          let duration = animator.get_frame_duration(&frame_data);
          let offset = animator.timer.excess();
          animator.timer = AnimationTimer::with_offset(duration, offset);

          if let Some(name) = Tag::log() {
            info!(
              "[{name}] remainder: {}, new timer: {:?}",
              offset, animator.timer,
            )
          }
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
    let Ok((source, look_dir)) = sources.get(target.animator_target) else {
      continue;
    };

    let current_state = source.state_machine.current_state();
    if_chain! {
      if let Some(animation) = target.animations.get(current_state);
      if let Some(frame_data) = animation.get(look_dir);
      if let Some(frame) = frame_data.frames.get(source.frame_index);
      then {
        sprite.flip_x = animation.flip_x(look_dir);
        if let Some(atlas) = &mut sprite.texture_atlas {
            atlas.index = frame.index;
        }
      }
    }
  }
}

#[derive(Component, Default, Reflect, Debug)]
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
macro_rules! var {
  ($var:ident) => {};
}
