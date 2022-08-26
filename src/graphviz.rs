#![allow(unused)]

pub use super::*;

impl<N, E> Nfa<N, E>
where
    N: std::fmt::Display,
    E: std::fmt::Display + Eq + Clone + std::hash::Hash + std::default::Default,
{
    pub fn graphviz_file(&self, filename: &str, label: &str) {
        use std::io::Write;
        let g = graphviz_wrap(self.graphviz(), label);
        let mut output = std::fs::File::create(filename).unwrap();
        assert!(output.write_all(g.as_bytes()).is_ok());
    }

    pub fn graphviz(&self) -> String {
        let mut ret = "".to_string();
        for (source, edges) in &self.transitions {
            for (target, edge) in edges {
                ret = format!(
                    r#"{ret}
  {} -> {} [label="{}" fontsize="20pt"];"#,
                    nodename(source),
                    nodename(target),
                    self.edge(edge)
                );
            }
        }
        for (id, node) in &self.nodes {
            let nodelabel = if self.entry.contains(id) {
                "enter".to_string()
            } else {
                node.to_string()
            };
            ret = format!(
                r#"{ret}
  {} [label="{}"]"#,
                nodename(id),
                nodelabel
            );
        }
        ret
    }
}

fn nodename(i: &NfaIndex) -> String {
    format!("node_{i}")
}

pub(crate) fn graphviz_wrap(s: String, label: &str) -> String {
    format!(
        r#"
strict digraph G {{
    rankdir = TB;
    remincross = true;
    splines = true;
    fontsize="40";
    label = "{label}";
    {}
}}
"#,
        s
    )
}
