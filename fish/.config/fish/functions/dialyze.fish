function dialyze
    mix do dialyzer.clean, dialyzer.build, dialyzer --format short
    afplay /System/Library/Sounds/Hero.aiff
end
