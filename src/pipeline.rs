use core::fmt::Debug;
use std::marker::PhantomData;

use crate::pretty::*;

pub trait Pass<Source, Target> {
    type Error;
    fn run(&self, input: Source) -> Result<Target, Self::Error>;
}

pub struct SimplePass<Source, Target, F: Fn(Source) -> Target> {
    _source: PhantomData<Source>,
    _target: PhantomData<Target>,
    run: F,
}

impl<Source, Target, F: Fn(Source) -> Target> SimplePass<Source, Target, F> {
    pub fn new(run: F) -> SimplePass<Source, Target, F> {
        SimplePass {
            _source: PhantomData,
            _target: PhantomData,
            run,
        }
    }
}

impl<Source, Target, F> Pass<Source, Target> for SimplePass<Source, Target, F>
where
    F: Fn(Source) -> Target,
{
    type Error = !;
    fn run(&self, input: Source) -> Result<Target, !> {
        Ok((self.run)(input))
    }
}

impl<Source, Target, F> From<F> for SimplePass<Source, Target, F>
where
    F: Fn(Source) -> Target,
{
    fn from(pass: F) -> Self {
        SimplePass::new(pass)
    }
}

pub struct FailliblePass<Source, Target, Error, F: Fn(Source) -> Result<Target, Error>> {
    _source: PhantomData<Source>,
    _target: PhantomData<Target>,
    _error: PhantomData<Error>,
    run: F,
}

impl<Source, Target, Error, F: Fn(Source) -> Result<Target, Error>>
    FailliblePass<Source, Target, Error, F>
{
    pub fn new(run: F) -> FailliblePass<Source, Target, Error, F> {
        FailliblePass {
            _source: PhantomData,
            _target: PhantomData,
            _error: PhantomData,
            run,
        }
    }
}

impl<Source, Target, Error, F> Pass<Source, Target> for FailliblePass<Source, Target, Error, F>
where
    F: Fn(Source) -> Result<Target, Error>,
{
    type Error = Error;
    fn run(&self, input: Source) -> Result<Target, Error> {
        (self.run)(input)
    }
}

impl<Source, Target, Error, F> From<F> for FailliblePass<Source, Target, Error, F>
where
    F: Fn(Source) -> Result<Target, Error>,
{
    fn from(pass: F) -> Self {
        FailliblePass::new(pass)
    }
}

pub struct InplacePass<Source, F: Fn(&mut Source)> {
    _source: PhantomData<Source>,
    run: F,
}

impl<Source, F: Fn(&mut Source)> InplacePass<Source, F> {
    pub fn new(run: F) -> InplacePass<Source, F> {
        InplacePass {
            _source: PhantomData,
            run,
        }
    }
}

impl<Source, F> Pass<Source, Source> for InplacePass<Source, F>
where
    F: Fn(&mut Source),
{
    type Error = !;
    fn run(&self, mut input: Source) -> Result<Source, !> {
        (self.run)(&mut input);
        Ok(input)
    }
}

impl<Source, F> From<F> for InplacePass<Source, F>
where
    F: Fn(&mut Source),
{
    fn from(pass: F) -> Self {
        InplacePass::new(pass)
    }
}

pub struct LogPrettyPass<'pl, Source, Target, Error> {
    pass: Box<dyn Pass<Source, Target, Error = Error> + 'pl>,
    name: &'static str,
    log: bool,
}

impl<'pl, Source, Target, Error> LogPrettyPass<'pl, Source, Target, Error> {
    pub fn new(
        name: &'static str,
        log: bool,
        pass: impl Pass<Source, Target, Error = Error> + 'pl,
    ) -> LogPrettyPass<'pl, Source, Target, Error> {
        LogPrettyPass {
            pass: box pass,
            name,
            log,
        }
    }
}

impl<'pl, Source, Target, Error> Pass<Source, Target> for LogPrettyPass<'pl, Source, Target, Error>
where
    for<'a> Target: PrettyPrec<'a> + Clone,
{
    type Error = Error;
    fn run(&self, input: Source) -> Result<Target, Error> {
        let target = self.pass.run(input)?;
        if self.log {
            let allocator = pretty::Arena::new();
            let doc = target.clone().pretty_prec(0, &allocator);
            println!("{}", self.name);
            doc.render_colored(
                80,
                termcolor::StandardStream::stdout(termcolor::ColorChoice::Auto),
            )
            .unwrap();
            println!();
        }
        Ok(target)
    }
}

pub struct LogDebugPass<'pl, Source, Target, Error> {
    pass: Box<dyn Pass<Source, Target, Error = Error> + 'pl>,
    name: &'static str,
    log: bool,
}

impl<'pl, Source, Target, Error> LogDebugPass<'pl, Source, Target, Error> {
    pub fn new(
        name: &'static str,
        log: bool,
        pass: impl Pass<Source, Target, Error = Error> + 'pl,
    ) -> LogDebugPass<'pl, Source, Target, Error> {
        LogDebugPass {
            pass: box pass,
            name,
            log,
        }
    }
}

impl<'pl, Source, Target, Error> Pass<Source, Target> for LogDebugPass<'pl, Source, Target, Error>
where
    Target: Debug,
{
    type Error = Error;
    fn run(&self, input: Source) -> Result<Target, Error> {
        let target = self.pass.run(input)?;
        if self.log {
            println!("{}\n{:#?}", self.name, target);
            println!();
        }
        Ok(target)
    }
}

pub trait Pipeline<'pl, Source, Target> {
    fn run(&self, input: Source) -> Target;
}

struct IdPipeline<'pl, Source> {
    _source: PhantomData<&'pl Source>,
}

impl<'pl, Source> Pipeline<'pl, Source, Source> for IdPipeline<'pl, Source> {
    fn run(&self, input: Source) -> Source {
        input
    }
}

struct PassPipeline<'pl, Source, Intermediate, Target, Error> {
    pipeline: Box<dyn Pipeline<'pl, Source, Intermediate> + 'pl>,
    pass: Box<dyn Pass<Intermediate, Target, Error = Error> + 'pl>,
}

impl<'pl, Source, Intermediate, Target, Error> Pipeline<'pl, Source, Target>
    for PassPipeline<'pl, Source, Intermediate, Target, Error>
where
    for<'a> Error: PrettyPrec<'a>,
{
    fn run(&self, input: Source) -> Target {
        let intermediate = self.pipeline.run(input);
        self.pass.run(intermediate).unwrap_or_else(|error| {
            let allocator = ::pretty::Arena::new();
            let doc = error.pretty_prec(0, &allocator);
            doc.render_colored(
                80,
                termcolor::StandardStream::stdout(termcolor::ColorChoice::Auto),
            )
            .unwrap();
            println!();
            std::process::exit(1)
        })
    }
}

pub struct PipelineBuilder<'pl, Source, Target> {
    pipeline: Box<dyn Pipeline<'pl, Source, Target> + 'pl>,
}

impl<'pl, Source: 'pl> PipelineBuilder<'pl, Source, Source> {
    pub fn new() -> PipelineBuilder<'pl, Source, Source> {
        PipelineBuilder {
            pipeline: box IdPipeline {
                _source: PhantomData,
            },
        }
    }
}

impl<'pl, Source, Intermediate> PipelineBuilder<'pl, Source, Intermediate> {
    pub fn then<Target, Error>(
        self,
        pass: impl Pass<Intermediate, Target, Error = Error> + 'pl,
    ) -> PipelineBuilder<'pl, Source, Target>
    where
        Source: 'pl,
        Intermediate: 'pl,
        Target: 'pl,
        for<'a> Error: PrettyPrec<'a> + 'pl,
    {
        PipelineBuilder {
            pipeline: box PassPipeline {
                pipeline: self.pipeline,
                pass: box pass,
            },
        }
    }
}

impl<'pl, Source, Target> PipelineBuilder<'pl, Source, Target> {
    pub fn build(self) -> Box<dyn Pipeline<'pl, Source, Target> + 'pl> {
        self.pipeline
    }
}
