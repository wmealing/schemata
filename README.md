# 🏛️ Schemata

## WARNING: This is alpha quality software, there are likely BETTER libraries out there than this.   

**Declarative, versioned database schema migrations in pure Erlang.**

Schemata helps you define, track, and apply database schema changes using straightforward, Erlang-friendly patterns. No external dependencies, no DSLs — just Erlang.

---

## ✨ Features

- ✅ Pure Erlang – no drivers or compilers required
- 📜 Declarative migration files (Erlang modules)
- 🕒 Version tracking and ordering
- 🔄 Idempotent, safe execution
- 🛠 Supports PostgreSQL, MySQL, and SQLite (pluggable adapters)
- ♻️ Rollback and re-run strategies

---

## 🚀 Getting Started

### 1. Add Schemata to your `rebar.config`

```erlang
{deps, [
    {schemata, {git, "https://github.com/wmealing/schemata", {tag, "0.1.0"}}}
]}.
```

2. Create a migration

```
$ erl erl _build/test/lib/schemata/src/new_migration.erl create_users_table
```

It should make a template file in your current projects:

src/migrations/
              └── 20250422_create_users_table.erl


It creates a template file.

```
-module(m20250422_create_users_table).
-export([up/1, down/1]).

up() ->
    "SQL CREATE COMMAND HERE"

down() ->
    "SQL ROLLBACK COMMAND HERE"

```

3. Run migrations (example with the sqlite_adapter).


```
schemata:migrate(DbConn, schemata_sqlite_adapter).
```

📁 Migration Structure

Each migration is an Erlang module that exports up/0 and down/0. Migrations are run in filename order based on their timestamp prefix.

You can use the built-in schemata_sqlite_adapter or plug in your own.

🔧 Configuration

Configure your database adapter and migration path in your local `config/sys.config`

```
ApplicationConfig = [
    {schemata, [
        {adapter, schemata_sqlite_adapter},
        {migration_dir, "migrations"}
    ]}
].

```

🧪 Testing

The current test suite runs using the sqlite adapter.

```
eunit:test(schemata).
```


🗺 Roadmap

 Database diffing & auto-generation
 Interactive CLI
 CI-safe dry-run mode

📜 License

MIT License. See LICENSE for details.

🧠 Inspiration

Schemata was inspired by tools like Ragtime, Flyway, and Sqitch — reimagined for the Erlang ecosystem.
