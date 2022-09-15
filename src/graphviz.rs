use super::*;

impl<M> Dfa<M>
where
    M: std::fmt::Debug + Clone + PartialOrd + Ord,
{
    #[tracing::instrument(skip_all)]
    pub fn graphviz_file(&self, filename: &str, label: &str) {
        #[cfg(feature = "graphs")]
        {
            use std::io::Write;
            let g = graphviz_wrap(self.graphviz(), label);
            let mut output = std::fs::File::create(filename).unwrap();
            assert!(output.write_all(g.as_bytes()).is_ok());
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn graphviz(&self) -> String {
        let mut ret = format!("root = {};\n", nodename(&self.entry));
        for (element, edges) in &self.transitions {
            for (source, targets) in edges {
                // ?
                for target in targets {
                    ret = format!(
                        r#"{ret}
    {} -> {} [label="{}" fontsize="20pt"];"#,
                        nodename(source),
                        nodename(target),
                        element
                    );
                }
            }
        }
        for id in &self.ids() {
            let nodelabel = if self.entry == *id {
                "enter".to_string()
            } else {
                format!("{:?}", id)
            };
            ret = format!(
                r#"{ret}
    {} [label="{}", shape="{}"]"#,
                nodename(id),
                nodelabel,
                match self
                    .states
                    .get(id)
                    .iter()
                    .map(|x| x.iter().any(|s| s.accepting()))
                    .any(|b| b)
                {
                    true => "doublecircle",
                    false => "circle",
                }
            );
        }
        ret
    }
}

fn nodename(i: &UnionedId) -> String {
    format!(
        "node_{}",
        i.iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("_")
    )
}

pub(crate) fn graphviz_wrap(s: String, label: &str) -> String {
    format!(
        r##"
digraph G {{
    rankdir = TB;
    remincross = true;
    splines = true;
    fontsize="40";

    bgcolor = "#555555";
    node[color = "#FFFFFF"];
    node[fontcolor = "#FFFFFF"];
    edge[color = "#FFFFFF", fontcolor="#FFFFFF"];

    label = "{label}";
    {}
}}
"##,
        s
    )
}
