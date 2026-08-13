---
# Rename this directory (and `name` below) to your skill's kebab-case name.
# The agent reads `description` to decide WHEN to invoke the skill, so make it
# action-triggering: say what it does AND when to use it. Mention any optional
# arguments here too.
name: template-skill
description: One-line summary of what this skill does and when to use it. Note any optional arguments and their defaults here (e.g. "Accepts an optional X argument, defaults to Y").
# Restrict the tools this skill may use. Omit the line entirely to allow all.
allowed-tools: Bash(git:*)
---

# Template Skill

## Parameters

- **arg name** *(optional)* — what it controls. Defaults to `<default>`. If the
  invocation includes an argument, substitute it wherever `<default>` appears
  below.

## Context

Describe the information to gather or the steps to take. Use fenced `bash`
blocks for commands the agent should run:

```bash
echo "example command"
```

## Your Task

State the concrete outcome the agent should produce from the context above.
