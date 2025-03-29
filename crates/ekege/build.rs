#![allow(missing_docs)]
use std::iter;

use ekege_artifact::{Artifact, ToTokens, TokenStream, TokenStreamExt, format_ident, quote};
use rand::{Rng, SeedableRng, seq::SliceRandom};
use rand_chacha::ChaCha12Rng;

const fn binomial_coefficient(mut n: u32, mut k: u32) -> u32 {
    let mut permutations = n;
    let mut orderings = k;

    while k > 1 {
        n -= 1;
        k -= 1;

        permutations *= n;
        orderings *= k;
    }

    permutations / orderings
}

const BLOOM_FILTER_TAG_ONE_BITS: u32 = 4;

const UNIQUE_BLOOM_FILTER_TAGS: u32 = binomial_coefficient(u16::BITS, BLOOM_FILTER_TAG_ONE_BITS);
const BLOOM_FILTER_TAGS: usize = UNIQUE_BLOOM_FILTER_TAGS.next_power_of_two() as usize;

const FIRST_BLOOM_FILTER_TAG: u16 = (1u16 << BLOOM_FILTER_TAG_ONE_BITS) - 1;
const LAST_BLOOM_FILTER_TAG: u16 = !0 << (u16::BITS - BLOOM_FILTER_TAG_ONE_BITS);

const RNG_SEED: u64 = 42;

fn get_rng() -> impl Rng {
    ChaCha12Rng::seed_from_u64(RNG_SEED)
}

struct BloomFilterTags(Vec<u16>);

impl ToTokens for BloomFilterTags {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let tags = &self.0;

        tokens.append_all(quote! { [#(#tags),*] });
    }
}

impl Artifact for BloomFilterTags {
    fn generate_type(&self) -> impl ToTokens {
        let length = self.0.len();

        quote! { [u16; #length] }
    }
}

fn generate_bloom_filter_tags(rng: &mut impl Rng) -> BloomFilterTags {
    let mut tags = iter::successors(Some(FIRST_BLOOM_FILTER_TAG), |&tag| {
        if tag == LAST_BLOOM_FILTER_TAG {
            return Some(FIRST_BLOOM_FILTER_TAG);
        }

        // This is a bit complicated, so I hope the descriptive names help.
        // Additionally, to visualize what output this code is trying to
        // achieve, take a look at the following list of consecutive tags:
        //
        // 0000001001100100
        // 0000001001101000
        // 0000001001110000
        // 0000001010000011
        let trailing_zero_filled_tag = tag | (tag - 1);
        let most_significant_one_of_least_significant_one_group_shifted_left_with_rest_of_tag =
            trailing_zero_filled_tag.wrapping_add(1);
        let shifted_back_rest_of_group = (tag
            ^ most_significant_one_of_least_significant_one_group_shifted_left_with_rest_of_tag)
            >> (tag.trailing_zeros() + 2);
        let next_tag =
            most_significant_one_of_least_significant_one_group_shifted_left_with_rest_of_tag
                ^ shifted_back_rest_of_group;

        Some(next_tag)
    })
    .take(BLOOM_FILTER_TAGS)
    .collect::<Vec<_>>();

    tags.shuffle(rng);

    BloomFilterTags(tags)
}

fn send_cargo_instructions() {
    // Don't default to scanning the whole package directory for changes: this
    // script is independent.
    println!("cargo::rerun-if-changed=build.rs");
}

fn main() {
    let mut rng = get_rng();

    ekege_artifact::store_artifact(
        format_ident!("BLOOM_FILTER_TAG"),
        generate_bloom_filter_tags(&mut rng),
    );

    send_cargo_instructions();
}
