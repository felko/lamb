use std::{fmt::Display, marker::PhantomData};

pub trait LambError = Display;
pub trait IR = Clone + Display;

type Result<'a, T> = std::result::Result<T, Box<dyn LambError + 'a>>;

pub trait Pass<'a, Source, Target> {
    fn run(&self, input: Source) -> Result<'a, Target>;
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

impl<'a, Source: 'a, Target: IR + 'a, F> Pass<'a, Source, Target> for SimplePass<Source, Target, F>
where
    F: Fn(Source) -> Target,
{
    fn run(&self, input: Source) -> Result<'a, Target> {
        Ok((self.run)(input))
    }
}

pub struct FailliblePass<Source, Target, Error, F: Fn(Source) -> std::result::Result<Target, Error>>
{
    _source: PhantomData<Source>,
    _target: PhantomData<Target>,
    _error: PhantomData<Error>,
    run: F,
}

impl<Source, Target, Error, F: Fn(Source) -> std::result::Result<Target, Error>>
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

impl<'a, Source: 'a, Target: IR + 'a, Error: LambError + 'a, F> Pass<'a, Source, Target>
    for FailliblePass<Source, Target, Error, F>
where
    F: Fn(Source) -> std::result::Result<Target, Error>,
{
    fn run(&self, input: Source) -> Result<'a, Target> {
        match (self.run)(input) {
            Ok(output) => Ok(output),
            Err(error) => Err(box error),
        }
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

impl<'a, Source: IR + 'a, F> Pass<'a, Source, Source> for InplacePass<Source, F>
where
    F: Fn(&mut Source),
{
    fn run(&self, mut input: Source) -> Result<'a, Source> {
        (self.run)(&mut input);
        Ok(input)
    }
}

pub trait Pipeline<'a, Source, Target> {
    fn run(&self, input: Source) -> Result<'a, Target>;
}

struct IdPipeline<'a, Source> {
    _source: PhantomData<&'a Source>,
}

struct PassPipeline<'a, Source, Intermediate, Target> {
    pipeline: Box<dyn Pipeline<'a, Source, Intermediate> + 'a>,
    pass: Box<dyn Pass<'a, Intermediate, Target> + 'a>,
}

impl<'a, Source> Pipeline<'a, Source, Source> for IdPipeline<'a, Source> {
    fn run(&self, input: Source) -> Result<'a, Source> {
        Ok(input)
    }
}

impl<'a, Source: 'a, Intermediate: IR + 'a, Target: IR + 'a> Pipeline<'a, Source, Target>
    for PassPipeline<'a, Source, Intermediate, Target>
{
    fn run(&self, input: Source) -> Result<'a, Target> {
        let intermediate = self.pipeline.run(input)?;
        self.pass.run(intermediate)
    }
}

pub struct PipelineBuilder<'a, Source, Target> {
    pipeline: Box<dyn Pipeline<'a, Source, Target> + 'a>,
}

impl<'a, Source: 'a> PipelineBuilder<'a, Source, Source> {
    pub fn new() -> PipelineBuilder<'a, Source, Source> {
        PipelineBuilder {
            pipeline: box IdPipeline {
                _source: PhantomData,
            },
        }
    }
}

impl<'a, Source: 'a, Intermediate: IR + 'a> PipelineBuilder<'a, Source, Intermediate> {
    pub fn add_pass<Target: IR + 'a>(
        self,
        pass: impl Pass<'a, Intermediate, Target> + 'a,
    ) -> PipelineBuilder<'a, Source, Target> {
        PipelineBuilder {
            pipeline: box PassPipeline {
                pipeline: self.pipeline,
                pass: box pass,
            },
        }
    }

    pub fn then<Target: IR + 'a, F: Fn(Intermediate) -> Target + 'a>(
        self,
        pass: F,
    ) -> PipelineBuilder<'a, Source, Target> {
        self.add_pass(SimplePass::new(pass))
    }

    pub fn then_try<
        Target: IR + 'a,
        Error: LambError + 'a,
        F: Fn(Intermediate) -> std::result::Result<Target, Error> + 'a,
    >(
        self,
        pass: F,
    ) -> PipelineBuilder<'a, Source, Target> {
        self.add_pass(FailliblePass::new(pass))
    }

    pub fn then_mut<F: Fn(&mut Intermediate) + 'a>(
        self,
        pass: F,
    ) -> PipelineBuilder<'a, Source, Intermediate> {
        self.add_pass(InplacePass::new(pass))
    }
}

impl<'a, Source: 'a, Target: IR + 'a> PipelineBuilder<'a, Source, Target> {
    pub fn build(self) -> Box<dyn Pipeline<'a, Source, Target> + 'a> {
        self.pipeline
    }
}
